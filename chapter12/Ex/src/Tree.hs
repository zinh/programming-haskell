module Tree where

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node left val right) = Node (fmap f left) (f val) (fmap f right)
