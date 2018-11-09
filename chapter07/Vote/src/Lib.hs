module Lib
    ( someFunc,
    count, rmdups, result, winner
    ) where

import Data.List (sort)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

count :: Eq a => a -> [a] -> Int
count item xs = sum [1 | x <- xs, item == x]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (x/=) (rmdups xs)

result :: Ord a => [a] -> [(Int, a)]
result xs = sort [(count c xs, c) | c <- rmdups xs]

winner :: Ord a => [a] -> a
winner = snd . last . result
