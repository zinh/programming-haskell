module Lib
    ( someFunc,
    factorial, sumdown,
    pow, gcd',
    length', init', drop'
    ) where

import Prelude hiding (gcd)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

factorial :: Int -> Int
factorial 1 = 1
factorial n 
  | n <= 0 = 0
  | otherwise = n * factorial(n - 1)

sumdown :: Int -> Int
sumdown n 
  | n <= 0 = 0
  | otherwise = n + sumdown(n - 1)

pow :: Int -> Int -> Int
pow _ 0 = 1
pow a n =  a * pow a (n - 1)

gcd' :: Int -> Int -> Int
gcd' a b
  | b <= 0 = a
  | a < b = gcd' b a
  | otherwise = gcd' (a - b) b

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

init' :: [a] -> [a]
init' (x:[]) = []
init' (x:xs) = x : (init' xs)

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) xs
