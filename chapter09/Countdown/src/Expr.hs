module Expr(Expr(..), eval) where

import Op (Op, apply, valid)

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App op x y) = bracket x ++ show op ++ bracket y
    where bracket (Val n) = show n
          bracket e = "(" ++ show e ++ ")"


values :: Expr -> [Int]
values (Val n) = [n]
values (App _ e1 e2) = values e1 ++ values e2

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App op left right) = [apply op x y | x <- eval left, y <- eval right, valid op x y]
