module Lib
    ( someFunc,
    sumOfSquares,
    grid, squareGrid,
    replicate',
    pyths, factor, isPerfect, perfects,
    scalarproduct
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

sumOfSquares :: Int -> Int
sumOfSquares n 
  | n <= 0 = 0
  | otherwise = sum [i*i | i <- [1..n]]

grid :: Int -> Int -> [(Int, Int)]
grid n m = [(x, y) |x <- [0..n], y <- [0..m]]

squareGrid :: Int -> [(Int, Int)]
squareGrid n = grid n n

replicate' :: Int -> a -> [a]
replicate' n item = [item | i <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [x..n], z <- [y..n], x*x + y*y == z*z]

factor :: Int -> [Int]
factor n = [d | d <- [1..n `div` 2], n `mod` d == 0]

isPerfect :: Int -> Bool
isPerfect n = sum (factor n) == n

perfects :: Int -> [Int]
perfects n = [i | i <- [2..n], isPerfect i]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
