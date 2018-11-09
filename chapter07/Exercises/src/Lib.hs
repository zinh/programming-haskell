module Lib
    ( someFunc,
    all, any, takeWhile,
    altMap
    ) where

import Prelude hiding (all, any, takeWhile)

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

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f xss@(x:xs)
  | f x = xss
  | otherwise = dropWhile' f xs

-- Ex 3
map :: (a -> b) -> [a] -> [b]
map f xs = foldr (\x memo -> f x : memo) [] xs

filter :: (a -> Bool) -> [a] -> [a]
filter f xs = foldr (\x memo -> if f x then x : memo else memo) [] xs

-- Ex 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f1 f2 (x:xs) = f1 x : altMap f2 f1 xs
