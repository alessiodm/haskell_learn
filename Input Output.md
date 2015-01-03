Input/Output
======================================

```
main = do
    foo <- putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")
```

Except for the last line, every line in a do block that doesn't bind can also be written with a bind (i.e., <- ).

```
import Data.Char  
  
main = do  
    putStrLn "What's your first name?"  
    firstName <- getLine  
    putStrLn "What's your last name?"  
    lastName <- getLine  
    let bigFirstName = map toUpper firstName  
        bigLastName = map toUpper lastName  
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
```

Use <- when you want to bind results of I/O actions to names and you can use let bindings to bind pure expressions to names.

```
main = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            main  
  
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words
```

The return in Haskell is really nothing like the return in most other languages! It has the same name, which confuses a lot of people, but in reality it's quite different. In imperative languages, return usually ends the execution of a method or subroutine and makes it report some sort of value to whoever called it. In Haskell (in I/O actions specifically), it makes an I/O action out of a pure value.

The following goes all the way to the last line:
```
main = do  
    return ()  
    return "HAHAHA"  
    line <- getLine  
    return "BLAH BLAH BLAH"  
    return 4  
    putStrLn line
```

We can use return in combination with <- to bind stuff to names. So you see, return is sort of the opposite to <-. While return takes a value and wraps it up in a box, <- takes a box (and performs it) and takes the value out of it, binding it to a name.
```
main = do  
   a <- return "hell"  
   b <- return "yeah!"  
   putStrLn $ a ++ " " ++ b 
```

Don't think of a function like putStrLn as a function that takes a string and prints it to the screen. Think of it as a function that takes a string and returns an I/O action. That I/O action will, when performed, print beautiful poetry to your terminal.


## Files and Streams

```
import System.IO  
  
main = do  
    handle <- openFile "girlfriend.txt" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle  
```

```
import System.IO     
    
main = do     
    withFile "girlfriend.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle     
        putStr contents) 
```

## Randomness

TODO


## Exceptions

Pure code can throw exceptions (e.g., 4 `div` 0), but it they can only be caught in the I/O part of our code (when we're inside a do block that goes into main). That's because you don't know when (or if) anything will be evaluated in pure code, because it is lazy and doesn't have a well-defined order of execution, whereas I/O code does.

```
import System.Environment     
import System.IO     
import System.IO.Error     
    
main = toTry `catch` handler     
                 
toTry :: IO ()     
toTry = do (fileName:_) <- getArgs     
           contents <- readFile fileName     
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"     
    
handler :: IOError -> IO ()     
handler e     
    | isDoesNotExistError e =   
        case ioeGetFileName e of Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path  
                                 Nothing -> putStrLn "Whoops! File does not exist at unknown location!"  
    | otherwise = ioError e
```
