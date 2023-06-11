{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-} 

-- import Data.Set   -- collide with Data.List 
import Control.Monad
import Data.Char
import qualified Data.List as L
import Data.List.Split
import Data.Time
import Data.Time.Clock.POSIX
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO hiding (putStrLn)
import System.Posix.Files
import System.Posix.Unistd
import System.Process
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdOutput)

import Text.Read
import Text.Regex
import Text.Regex.Base
import Text.Regex.Base.RegexLike
import Text.Regex.Posix
import Data.IORef 
import Control.Monad (unless, when)
import Control.Arrow ((***))
import Control.Concurrent 
import qualified Data.Aeson as DA

import GHC.Generics
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as I
import qualified Data.Text as TS
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson (ToJSON, decode, encode)

import qualified Data.ByteString               as B
import qualified Data.ByteString.Internal      as BI
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BLI

import qualified Network.Wai.Handler.Warp as WARP
import Network.HTTP.Types
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Header
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Client


import AronModule 
import AronAlias

{-|
    === Take the first CodeBlock
-}
firstCodeBlock :: [String] -> [String]
firstCodeBlock cx = isH ? lt $ error "Invalid Snippet Format"
    where
      lr    = trimBothEnd cx
      isH   = len lr > 1 ? (isHeaderFstLine $ head lr) $ error "Invalid Snippet Format"
      -- ls    = filter (\x -> len x > 0) $ map trimList $ splitWhen isHeaderFstLine cx
      ls    = filter (\x -> len x > 0) $ map trimBothEnd $ splitWhen isHeaderFstLine cx
      lt    = (take 1 lr) ++ (head ls)

take2 :: String -> [String]
take2 [] = []
take2 s | len s == 1 = [s]
take2 (a:b:cx) = [a,b] : take2 (b:cx)
            
{-|
    === Check whether the first line is the header with pid 

    @
        ab:*:cd  @= 123
    @
-}
isHeaderFstLine :: String -> Bool
isHeaderFstLine [] = False
isHeaderFstLine cx = c2 == 2 && d1 == 1 
    where 
      c2 = sum $ map (\x -> x == ':' ? 1 $ 0) cx
      d1 = sum $ map (\x -> x == "@=" ? 1 $ 0) $ take2 cx
      lt = join (***) trim $ let ma = splitWhenFirstNoRegex "@=" cx in case ma of
                                                                           Just x -> x
                                                                           Nothing -> error "ERROR: splitWhenFirstNoRegex"
      num = snd lt 
      isNum = foldr (\a b -> a && b) True $ map isDigit num

{-|
    ===

    @
        Takes the first line and parse it, if the header is valid, return (True, header, 122), else (False, "", 0)
                                                                                          ↑ 
                                                                                          + PID number in table CodeBlock
    @
-}
extractHeadX :: String -> (Bool, String, Integer)
extractHeadX [] = (False, [], 0)
extractHeadX st = isHeaderFstLine st ? ( let s = join (***) trim $ let ma = splitWhenFirstNoRegex "@=" st in case ma of
                                                                                                              Just x -> x
                                                                                                              Nothing -> error "ERROR: extractHeadX"
                                                                 
                                         in let sn = snd s in (True, fst s, read sn :: Integer)) $ (False, [], 0)

isHeaderT :: TS.Text -> Bool
isHeaderT cs = isHeaderFstLine cx' 
   where 
      cx' = strictTextToStr cs

postCodeBlockX :: String -> [String] -> IO()
postCodeBlockX url lt = do
                if len lt > 1 then do
                    let tu@(isH, fstLine, pid) = extractHeadX $ head lt 
                    fw "tu"
                    pre tu 
                    if isH then do
                        manager <- newManager defaultManagerSettings
                        let tt = tail lt 
                        let code = unlines $ fstLine:tt 
                        fw "code"
                        pre code 
                        let block = UpdateCodeBlock{pid = pid, newcode = code, begt = 0, endt = 0} 
                        fw "block"
                        pre block
                        -- initReq <- parseRequest "http://localhost:8081/updatecode"
                        initReq <- parseRequest url 
                        let req = initReq { method = "POST", requestBody = RequestBodyLBS $ encode block}
                        response <- httpLbs req manager
                        putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
                        print $ responseBody response
                    else do
                        pp "ERROR: postCodeBlockX"
                else do
                    pp "ERROR: Code Block have at lease two lines"

putColor n s = putStrLn $ colorfgStr n s

fun2 :: [(Char, Int)] -> String -> [(Char, Int)]
fun2 [] _         = []
fun2 (a:b:cx) pat = if fs == '@' && sn == '=' then [a, b] else fun2 (b:cx) pat
    where
        fs = fst a
        sn = fst b


split2Char :: String -> (Bool, String, String)
split2Char s = null at ? (False, "", "")  $ (True, take hn s, drop lm s)
    where 
        tu = zip s [0..]

        at = fun tu "@="
        hn = (not . null) at ? (snd $ head at) $ 0
        lm = (not . null) at ? (let k = snd $ last at in k + 1) $ 0

        fun :: [(Char, Int)] -> String -> [(Char, Int)]
        fun [] _         = []
        fun s _ | len s == 1 = [] 
        fun (a:b:cx) pat = if fst a == '@' && fst b == '=' then  a : b : (fun cx pat) else fun (b:cx) pat

{-|
    join :: (Monad m) => m (m a)
    
    join :: (a -> a -> b) -> (a -> b)
    join    (a -> ((->) a b)) -> ((->) a b)
    join    ((a ->) ((a->) b)) -> ((a ->) b)
               x        x              x
            (x -> x -> b) -> (x -> b)

    (***) = \f g (a, b) -> (f a, g b)
    (***) = \f f (a, a) -> (f a, f a)
    (***) = f -> f -> (a, a) -> (b, b)

    join :: (a -> a -> b) -> (a -> b)
           (a, a) -> b -> (a -> b)

    join $ (***) f (a, a)

    join (***) 

-}

trimBothEnd = g . f 
  where
    f ls = dropWhile (\x -> (len . trim) x == 0) ls 
    g = reverse . f . reverse

main = do
        argList <- getArgs
        case len argList of
            v | v == 2 -> do
                let url = head argList 
                let tmpfile = last argList
                ls <- readFileSTextList tmpfile >>= \x -> return $ map strictTextToStr x
                fw "ls"
                pre ls
                
                -- let lt = trimList ls
                let lt = trimBothEnd ls
                fw "lt"
                pre lt
                let lt' = firstCodeBlock lt
                fw "lt'"
                pre lt'

                postCodeBlockX url lt' 
                -- END_out1
              | v == 1 -> do
                let url = head argList 
                -- pipe as stdInput
                ls <- getContents >>= return . lines
                fw "ls"
                pre ls
                -- let lt = trimList ls
                let lt = trimBothEnd ls
                fw "lt"
                pre lt
                postCodeBlockX url lt
              | otherwise -> do
                putColor 140 "\tTwo arguments :"
                putColor 200 "\tpostSnippet http://localhost:8081/updatecode  /tmp/x1.x                                 "
                putColor 100 "\t                     ↑                            ↑                                     "
                putColor 100 "\t                     + -> Update code URL          + -> tmp file store edited snippet    "
                putColor 100 "\t"
                putColor 100 "\t"
                putColor 140 "\tOne arguments => Stdin from Pipe"
                putColor 200 "\tcat /tmp/x1.x | postSnippet http://localhost:8081/updatecode                             "
                putColor 100 "\t"
                putColor 130 "\tThere are at least two lines in an input file"
                putColor 100 "\t"
                putColor 140 "\tSee => HttpSnippetX() and UpdateSnippet()"
                putColor 100 "\tVerson: 1.0"
                putColor 100 "\t"
