module Lib
    ( someFunc, choices,
    solution, split
    ) where

import Expr (Expr, eval, values)

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
solution expr ns result = eval expr == [result] && (values expr) `elem` (choices ns)

-- Split a list into 2 non-empty sublists
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

-- return all posible expressions
-- whose list of values is precisely a given list
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
           l <- exprs ls,
           r <- exprs rs,
           e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine e1 e2 = error "not implemented"
