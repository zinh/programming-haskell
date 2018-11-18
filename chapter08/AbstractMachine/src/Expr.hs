module Expr where

data Expr = Val Int | Add Expr Expr

type Cont = [Op]

data Op = EVAL Expr | ADD Int

value :: Expr -> Int
value Val n = n
value (Add e1 e2) = (value e1) + (value e2)

eval :: Expr -> Cont -> Int
eval (Val n) c =  exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL e : c) n = eval e (ADD n : c)
exec (ADD m : c) n = exec c (n + m)
