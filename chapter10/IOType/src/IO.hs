module IO where

import System.IO (hSetEcho, stdin)
import Prelude hiding(getLine, putStr, putStrLn)

getCh :: IO(Char)
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

getLine :: IO String
getLine = do
  x <- getChar
  if x == '\n' then
               return []
               else
                 do xs <- getLine
                    return (x:xs)

putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do
  putChar x
  putStr xs

putStrLn :: String -> IO ()
putStrLn xs = do
  putStr xs
  putChar '\n'

strlen :: IO ()
strlen = do
  putStr "Enter a string: "
  xs <- getLine
  putStrLn ("The length is: " ++ show (length xs))

-- Clear screen
cls :: IO ()
cls = putStr "\ESC[2J"
