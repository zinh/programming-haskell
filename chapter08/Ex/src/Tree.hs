module Tree(Tree(..), balanced, balance) where

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

countLeaf :: Tree a -> Int
countLeaf (Leaf _) = 1
countLeaf (Node left right) = countLeaf left + countLeaf right

balanced :: Tree a -> Bool
balanced (Node left right) =
  let leftLeaf = countLeaf left
      rightLeaf = countLeaf right
   in abs (leftLeaf - rightLeaf) <= 1 

halve :: [a] -> ([a], [a])
halve lst =
  let l = (length lst) `div` 2
   in (take l lst, drop l lst)

balance :: [a] -> Tree a
balance [] = error "Empty array"
balance (x:[]) = Leaf x
balance lst =
  let (left, right) = halve lst
   in Node (balance left) (balance right)
