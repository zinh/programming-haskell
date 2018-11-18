module Lib
    ( someFunc,
    Prop(..),
    isTaut
    ) where

import Assoc (Assoc, find)

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

type Subt = Assoc Char Bool

someFunc :: IO ()
someFunc = putStrLn "someFunc"

eval :: Subt -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p1 p2) = (eval s p1) && (eval s p2)
eval s (Imply p1 p2) = (eval s p1) <= (eval s p2)

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var c) = [c]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = []
bools 1 = [[True], [False]]
bools n =
  let xs = bools (n - 1)
   in [True:x | x <- xs] ++ [False : x | x <- xs]

substs :: Prop -> [Subt]
substs p =
  let vs = vars p
      bs = bools (length vs)
   in map (zip vs) bs

isTaut :: Prop -> Bool
isTaut p = and [eval s p |s <- substs p]
