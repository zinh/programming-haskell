module Hangman where

import IO (getCh)

hangman :: IO ()
hangman = do
  putStrLn "Think of a word"
  word <- sgetLine
  putStrLn "Try to guess it"
  play word

sgetLine :: IO String
sgetLine = do
  x <- getCh
  if x == '\n' then
                 do putChar x
                    return []
               else
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)

play :: String -> IO()
play word = do putStr "?"
               guess <- getLine
               if guess == word then
                                putStrLn "You got it!!"
                                else
                                  do putStrLn (match word guess)
                                     play word


match :: String -> String -> String
match word guess = [if w `elem` guess then w else '-' | w <- word]
