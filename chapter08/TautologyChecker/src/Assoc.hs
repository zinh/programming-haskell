module Assoc (Assoc, find) where

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k [] = error "Not found"
find key ((k, v):t)
  | key == k = v
  | otherwise = find key t

--insert :: Eq k => k -> v -> Assoc k v -> Assoc k v
