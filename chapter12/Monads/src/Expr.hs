module Expr where

data Expr = Val Int | Div Expr Expr

-- Version 1
eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div e1 e2) = 
  case eval e1 of
    Nothing -> Nothing
    Just a -> case eval e2 of
                Nothing -> Nothing
                Just b ->  safediv a b

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv a b = Just (div a b)

eval2 :: Expr -> Maybe Int
eval2 (Val n) = Just n
eval2 (Div e1 e2) = do
  n <- eval e1
  m <- eval e2
  safediv n m
