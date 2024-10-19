{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-} 
{-# LANGUAGE QuasiQuotes #-} -- support raw string [r|<p>dog</p> |]

module Main where
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
import qualified Data.ByteString.UTF8          as BSU

import qualified Network.Wai.Handler.Warp as WARP
import Network.HTTP.Types
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Header
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Client
import Text.RawString.QQ (r)         -- Need QuasiQuotes too 


import AronModule 
import AronAlias

{-|
    === Take the first CodeBlock
-}
firstCodeBlock :: [String] -> [String]
firstCodeBlock cx = isH ? lt $ error "Invalid Snippet Format"
    where
      lr    = trimBothEnd cx
      isH   = len lr > 1 ? (isValidHeader $ head lr) $ error "Invalid Snippet Format"
      -- ls    = filter (\x -> len x > 0) $ map trimList $ splitWhen isValidHeader cx
      ls    = filter (\x -> len x > 0) $ map trimBothEnd $ splitWhen isValidHeader cx
      lt    = (take 1 lr) ++ (head ls)

take2 :: String -> [String]
take2 [] = []
take2 s | len s == 1 = [s]
take2 (a:b:cx) = [a,b] : take2 (b:cx)

  
{-|
    === Check whether the first line is the header with pid

    @
        ab:*:cd  @= 123 => True
        ab-c:*: abc     => False
        ab_c:*: abc     => True

        [a-zA-Z0-9_]+:*: whatever => True

        -- Old isValidHeader => True
        sed 's/[^[:print:]]\[[^a-zA-Z]*[a-zA-Z]//g' $t

        -- New isValidHeader => False
        sed 's/[^[:print:]]\[[^a-zA-Z]*[a-zA-Z]//g' $t
    @
-}
isValidHeader :: String -> Bool
isValidHeader [] = False
isValidHeader cx = (b1 && c2 == 2 && d1 == 1) || (b1 && c2 == 2)
    where
      s1 = splitStrCharNoRegex ":" cx
      b1 = if len s1 == 3 then let h = head s1 in hasWordChar h else False
      c2 = sum $ map (\x -> x == ':' ? 1 $ 0) cx
      d1 = sum $ map (\x -> x == "@=" ? 1 $ 0) $ take2 cx
      lt = join (***) trim $ let ma = splitWhenFirstNoRegex "@=" cx in case ma of
                                                                           Just x -> x
                                                                           Nothing -> error "ERROR: splitWhenFirstNoRegex"
      num = snd lt 
      isNum = foldr (\a b -> a && b) True $ map isDigit num


extractHead :: String -> (Bool, String, Integer)
extractHead [] = (False, [], 0)
extractHead cx = tu 
    where 
      b1 = let n = sum $ map (\x -> x == ':' ? 1 $ 0) cx in n == 2
      b2 = let n = sum $ map (\x -> x == "@=" ? 1 $ 0) $ take2 cx in n == 1

      tu = b1 && b2 ? (let s = join (***) trim $ let ma = splitWhenFirstNoRegex "@=" cx in case ma of
                                                                                          Just x -> x
                                                                                          Nothing -> error "ERROR: extractHeadX"
                                             
                       in let sn = snd s in (True, fst s, read sn :: Integer)) $  (b1 ?  (True, cx, 0) $ (False, [], 0))

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
extractHeadX st = isValidHeader st ? ( let s = join (***) trim $ let ma = splitWhenFirstNoRegex "@=" st in case ma of
                                                                                                              Just x -> x
                                                                                                              Nothing -> error "ERROR: extractHeadX"
                                                                 
                                         in let sn = snd s in (True, fst s, read sn :: Integer)) $ (False, [], 0)
                                         --                   a:*:b@=3
                                         --                   (True, a:*:b, 3)

isHeaderT :: TS.Text -> Bool
isHeaderT cs = isValidHeader cx' 
   where 
      cx' = strictTextToStr cs

{-|
 - If the first line has no @= then, it is 'insertcode'
 -                                   else  'updatecode' 
 -}
postCodeBlockX :: String -> [String] -> IO()
postCodeBlockX url lt = do
                if len lt > 1 then do
                    -- let tu@(isH, fstLine, pid) = extractHeadX $ head lt 
                    let tu@(isH, fstLine, pid) = extractHead $ head lt 
                    fw "tu"
                    pre tu 
                    if isH then do
                        manager <- newManager defaultManagerSettings
                        let tt = tail lt 
                        let code = unlines $ fstLine:tt 
                        fw "code"
                        pre code 
                        let cmd = pid == 0 ? "insertcode" $ "updatecode"
                        let furl = url </> cmd
                        let block = UpdateCodeBlock{pid = pid, pidlist = [], newcode = code, begt = 0, endt = 0} 
                        fw "block"
                        pre block
                        -- initReq <- parseRequest "http://localhost:8081/updatecode"
                        initReq <- parseRequest furl 
                        let req = initReq { method = BSU.fromString "POST", requestBody = RequestBodyLBS $ encode block}
                        response <- httpLbs req manager
                        putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
                        print $ responseBody response
                    else do
                        pp "ERROR: postCodeBlockX"
                else do
                    pp "ERROR: Code Block have at lease two lines"

deleteCodeBlock :: String -> [Integer] -> IO()
deleteCodeBlock url pidls = do
                            manager <- newManager defaultManagerSettings
                            let cmd = "deletecode"
                            let furl = url </> cmd
                            let block = UpdateCodeBlock{ pid = 0, pidlist = pidls, newcode = "", begt = 0, endt = 0 } 
                            fw "block"
                            pre block
                            -- initReq <- parseRequest "http://localhost:8081/updatecode"
                            initReq <- parseRequest furl 
                            let req = initReq { method = BSU.fromString "POST", requestBody = RequestBodyLBS $ encode block }
                            response <- httpLbs req manager
                            putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
                            print $ responseBody response

deleteCodeBlockX :: String -> [Integer] -> IO()
deleteCodeBlockX url pidls = do
                            manager <- newManager defaultManagerSettings
                            let cmd = "deletecode"
                            let furl = url </> cmd
                            let block = UpdateCodeBlock{ pid = 0, pidlist = pidls, newcode = "", begt = 0, endt = 0 } 
                            fw "block"
                            pre block
                            -- initReq <- parseRequest "http://localhost:8081/updatecode"
                            initReq <- parseRequest furl 
                            let req = initReq { method = BSU.fromString "POST", requestBody = RequestBodyLBS $ encode block }
                            response <- httpLbs req manager
                            putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
                            print $ responseBody response


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
            v | v == 30 -> do
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
                let a1 = head argList
                let a2 = last argList
                let urlx = a1 == "-d" ? "http://localhost:8080" $ (a1 == "-t" ? "http://localhost:8081" $ "")
                ls <- getContents >>= return . lines
                -- let lt = trimList ls
                let lt = trimBothEnd ls
                fw "lt"
                pre lt
                postCodeBlockX urlx lt

              | v == 2 && head argList == "-u" -> do
                let url = (head . tail) argList
                let tmpfile = last argList
                -- ls <- readFileSTextList tmpfile >>= \x -> return $ map strictTextToStr x
                ls <- getContents >>= return . lines
                let lt = trimBothEnd ls
                let lt' = firstCodeBlock lt
                postCodeBlockX url lt' 

              -- postSnippet -d /tmp/x1.x
              -- postSnippet -t /tmp/x1.x
              | v == 2 && (head argList == "-d" || head argList == "-t")  -> do
                let a1 = head argList
                let urlx = a1 == "-d" ? "http://localhost:8080" $ (a1 == "-t" ? "http://localhost:8081" $ "")
                let tmpfile = last argList
                ls <- readFileSTextList tmpfile >>= \x -> return $ map strictTextToStr x
                -- ls <- getContents >>= return . lines
                let lt = trimBothEnd ls
                let lt' = firstCodeBlock lt
                postCodeBlockX urlx lt' 

              | v >= 2 && (head argList == "-del")  -> do
                let urlx = "http://localhost:8080"
                -- let pid = read (last argList) :: Integer
                let pidlist = map (\x -> read x :: Integer) $ tail argList
                deleteCodeBlock urlx pidlist 

              | v == 10 -> do
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
                putColor 150 "\tpostSnippet -d  /tmp/a  => -d [default] => http://localhost:8080"
                putColor 150 "\tpostSnippet -t  /tmp/a  => -t [test]    => http://localhost:8081"
                putColor 170 "\techo /tmp/a | postSnippet -d"
                putColor 170 "\techo /tmp/a | postSnippet -t"
                putColor 100 ""
                putColor 100 "\t("
                putColor 100 "\t)"
                putColor 140 "\tTwo arguments :"
                putColor 200 "\tpostSnippet http://localhost:8081               /tmp/x1.x                                 "
                putColor 100 "\t                     ↑                            ↑                                     "
                putColor 100 "\t                     + -> Update code URL          + -> tmp file store edited snippet    "
                putColor 100 "\t"
                putColor 100 "\t"
                putColor 140 "\tOne arguments => Stdin from Pipe"
                putColor 200 "\tcat /tmp/x1.x | postSnippet http://localhost:8081                             "
                putColor 100 "\t"
                putColor 130 "\tInput file must contains at least two lines"
                putColor 100 "\t"
                putColor 140 "\tSee => HttpSnippetX() and UpdateSnippet()"
                putColor 100 "\tVerson: 1.0"
                putColor 100 "\t"
                let s = [r| 
                            postSnippet -del 10 20 30               => Delete pids = 10 20 30
                            postSnippet -d  /tmp/a  => -d [default] => http://localhost:8080
                            postSnippet -t  /tmp/a  => -t [test]    => http://localhost:8081
                            echo /tmp/a | postSnippet -d
                            echo /tmp/a | postSnippet -t

                            Bash
                            (
                            echo 'a:*: test'
                            echo 'line 1'
                            ) | postSnippet -d
                            
                            Two arguments :
                            postSnippet http://localhost:8081               /tmp/x1.x                                 
                                                 ↑                            ↑                                    
                                                 + -> Update code URL          + -> tmp file store edited snippet    
                            
                            One arguments => Stdin from Pipe
                            cat /tmp/x1.x | postSnippet http://localhost:8081                             
                            
                            Input file must contains at least two lines
                            
                            See => HttpSnippetX() and UpdateSnippet()
                        |] 
                let ss = lines s
                mapM_ putStrLn ss 
{--
main = do
       pp "ok"
       when True $ do
           let s = "a::b"
           print $ extractHead s
       when True $ do
           let s1 = "a::b@=3"
           print $ extractHead s1
       when True $ do
           let s1 = "a:b@=3"
           print $ extractHead s1
--}
