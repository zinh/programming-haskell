module Machine where

import Game -- (Grid, Player(..), size, next, full, move, won, wins)

data Tree a = Node a [Tree a] deriving Show

depth :: Int
depth = 9

gametree :: Grid -> Player -> Tree Grid
gametree gs p = Node gs [gametree g' (next p) | g' <- moves gs p]

moves :: Grid -> Player -> [Grid]
moves gs p
  | won gs = []
  | full gs = []
  | otherwise = concat [move gs i p | i <- [0..(size^2 - 1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node n _) = Node n []
prune depth (Node n ts) = Node n [prune (depth -1) t | t <- ts]

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node gs [])
  | wins X gs = Node (gs, X) []
  | wins O gs = Node (gs, O) []
  | otherwise = Node (gs, B) []

minimax (Node gs ts)
  | turn gs == O = Node (gs, minimum ps) ts'
  | turn gs == X = Node (gs, maximum ps) ts'
  where ts' = map minimax ts
        ps = [p | Node (_, p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove gs p = head [g' | Node (g', p') _ <- ts, p' == best]
  where tree = prune depth (gametree gs p)
        Node (_, best) ts = minimax tree
