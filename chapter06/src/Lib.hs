module Lib
    ( someFunc,
    factorial, sumdown,
    pow, gcd',
    length', init', drop',
    concat', and', replicate',
    idx, merge, msort,
    last', take', sum'
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

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && (and' xs)

concat' :: [a] -> [a] -> [a]
concat' xs [] = xs
concat' xs (y:ys) = concat' (xs ++ [y]) ys

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a : replicate' (n-1) a

idx :: [a] -> Int -> a
idx (x:xs) 0 = x
idx (x:xs) n = idx xs (n-1)
idx _ _ = error "Index is too large"

-- Merge sort
merge :: Ord a => [a] -> [a] -> [a]
merge xs ys = reverse (merge' xs ys [])

merge' :: Ord a => [a] -> [a] -> [a] -> [a]
merge' [] ys result = (reverse ys) ++ result 
merge' xs [] result = (reverse xs) ++ result
merge' xss@(x:xs) yss@(y:ys) result
  | x <= y = merge' xs yss (x:result)
  | otherwise = merge' xss ys (y:result)

halve :: [a] -> ([a], [a])
halve xs =
  let l = length xs
      middle = l `div` 2
   in (take middle xs, drop middle xs)

msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = [x]
msort xs =
  let (h, t) = halve xs
   in merge (msort h) (msort t)

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : (take' (n - 1) xs)

last' :: [a] -> a
last' [] = error "empty list"
last' (x:[]) = x
last' (x:xs) = last' xs
