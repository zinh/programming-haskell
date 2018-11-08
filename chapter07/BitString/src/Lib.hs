module Lib
  ( someFunc,
    transmit
  ) where

import Data.Char (ord, chr)

type Bit = Int

someFunc :: IO ()
someFunc = putStrLn "someFunc"

bin2int :: [Bit] -> Int
bin2int bits = foldr (\b memo -> b + 2*memo) 0 bits

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = (n `mod` 2) : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : (chop8 (drop 8 bits))

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id
