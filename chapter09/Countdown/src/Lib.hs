module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

interleave :: a -> [a] -> [[a]]
interleave x xs
