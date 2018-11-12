module Lib
    ( someFunc,
    all, any, takeWhile, dec2int, curry', uncurry'
    ) where

import Prelude hiding (all, any, takeWhile, dropWhile)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Ex 2
all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all f (x:xs) = f x && (all f xs)

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any f (x:xs) = f x || any f xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x:xs)
  | f x = x : (takeWhile f xs)
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f xss@(x:xs)
  | f x = xss
  | otherwise = dropWhile f xs

-- Ex 3
map :: (a -> b) -> [a] -> [b]
map f xs = foldr (\x memo -> f x : memo) [] xs

filter :: (a -> Bool) -> [a] -> [a]
filter f xs = foldr (\x memo -> if f x then x : memo else memo) [] xs

dec2int :: [Int] -> Int
dec2int = foldl (\memo digit -> digit + 10*memo) 0

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \a b -> f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(a, b) -> f a b

unfold' p h t x
  | p x = []
  | otherwise = h x : unfold' p h t (t x)
