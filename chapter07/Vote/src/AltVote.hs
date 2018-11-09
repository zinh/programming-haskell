module AltVote
  (rmempty) where

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])
