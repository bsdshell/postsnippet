# postSnippet
### Support commands
## Input from file
## Input from pipe

# postSnippet to http://localhost:8080 or http://localhost:8081
```
    # Input from file 
    # /tmp/kkk.x contains snippet
    postSnippet http://localhost:8081/insertcode  /tmp/kkk.x

    # Input from pipe
    echo "a:*:abc\n test1 \n test2\n" | postSnippet http://localhost:8081
```
Sat 10 Jun 11:46:50 2023 
* Fixed: Prefix spaces are removed by mistake
