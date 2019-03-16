module Tree(rlabel) where

import State

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

instance Functor Tree where
  fmap f (Leaf n) = Leaf (f n)
  fmap f (Node left right) = Node (fmap f left) (fmap f right)

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n + 1)
rlabel (Node left right) n = 
  let (labeledLeft, n') = rlabel left n
      (labeledRight, n'') =  rlabel right n'
   in (Node labeledLeft labeledRight, n'')

fresh :: ST Int
fresh = S (\n -> (n, n + 1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node left right) = Node <$> alabel left <*> alabel right

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do n <- fresh
                     return (Leaf n)

mlabel (Node left right) = do l' <- mlabel left
                              r' <- mlabel right
                              return (Node l' r')
