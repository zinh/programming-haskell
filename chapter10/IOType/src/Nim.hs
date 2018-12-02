module Nim where

import Data.Char (isDigit, digitToInt)
type Board = [Int]

next :: Int -> Int
next 1 = 2
next 2 = 1

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (==0)

valid :: Board -> Int -> Int -> Bool
valid board row star = star <= board !! (row - 1)

move :: Board -> Int -> Int -> Board
move (x:xs) 1 star = (x - star) : xs
move (x:xs) row star = x : move xs (row - 1) star

putRow :: Int -> Int -> IO ()
putRow row star = do putStr (show row ++ ". ")
                     putStrLn (replicate star '*')

putBoard :: Board -> IO()
putBoard board =
  let boardIdx = zip board [1..]
      putBoard' [] = return ()
      putBoard' ((star, row):xs) = do putRow row star
                                      putBoard' xs
   in putBoard' boardIdx

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newLine
                     if isDigit x then return (digitToInt x)
                                  else
                                    do
                                      putStrLn "Not a digit"
                                      getDigit prompt

newLine :: IO ()
newLine = putChar '\n'

play :: Board -> Int -> IO ()
play board player = do newLine
                       putBoard board
                       if finished board then
                                   do newLine
                                      putStr "Player "
                                      putStr (show (next player))
                                      putStrLn " wins!!"
                       else
                         do newLine
                            putStr "Player "
                            print player
                            row <- getDigit "Enter a row number: "
                            star <- getDigit "Stars to remove: "
                            if valid board row star then
                                                    play (move board row star) (next player)
                                                    else
                                                      do putStrLn "Invalid digit"
                                                         play board player
