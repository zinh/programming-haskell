module Lib
    ( someFunc,
    all, any, takeWhile, dec2int, curry', uncurry',
    altMap, unfold, chop8
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

dec2int :: [Int] -> Int
dec2int = foldl (\memo digit -> digit + 10*memo) 0

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \a b -> f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(a, b) -> f a b

--- Ex 6
--unfold :: (b -> Bool) -> (a -> b) -> (a -> b) -> a -> [b]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

chop8 :: [a] -> [[a]]
chop8 = unfold null (take 8) (drop 8)
-- map' f = unfold (==[]) f tail

-- Ex 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f1 f2 (x:xs) = f1 x : altMap f2 f1 xs
