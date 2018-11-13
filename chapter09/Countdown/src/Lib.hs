module Lib
    ( someFunc, choices
    ) where

import Expr (Expr, eval)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : [y:i | i <- interleave x ys]

-- Permutation
perms :: [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms (x:xs) = [y | ys <- perms xs, y <- interleave x ys]

-- Subset
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) =
  let subsn = subs xs
   in subsn ++ [x:xss | xss <- subsn]

choices :: [a] -> [[a]]
choices = concat . map perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution expr ns result = eval expr == result
