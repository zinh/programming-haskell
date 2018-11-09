module AltVote
  (rmempty, elim, rank, winner') where

import Data.List (sort)
import Lib (rmdups)

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim c = map (filter (/= c))

-- count 1st choice
count :: Eq a => a -> [[a]] -> Int
count c xs = sum [1 | x <- xs, head x == c]

rank :: Ord a => [[a]] -> [a]
rank xs = map snd (sort [(count x xs, x) | x <- rmdups (map head xs)])

winner' :: Ord a => [[a]] -> a
winner' xss =
  case rank (rmempty xss) of
    [c] -> c
    (x:_) -> winner' (elim x xss)
