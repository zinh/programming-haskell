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
chop8 = chop 8

appendParity :: [Bit] -> [Bit]
appendParity bits =
  let n1bits = sum [bit | bit <- bits, bit == 1]
   in if odd n1bits then 1 : bits else 0 : bits

validateParity :: [Bit] -> Maybe [Bit]
validateParity bits =
  let n1bits = sum [bit | bit <- drop 1 bits, bit == 1]
      parityBit = head bits
   in if (odd n1bits && parityBit == 1) || (even n1bits && parityBit == 0) then Just (drop 1 bits) else Nothing

chop :: Int -> [Bit] -> [[Bit]]
chop _ [] = []
chop n bits = take n bits : (chop n (drop n bits))

encode :: String -> [Bit]
encode = concat . map (appendParity . make8 . int2bin . ord)

decode :: [Bit] -> String
decode bits = 
  let results = sequence $ fmap (chr . bin2int) <$> (map validateParity (chop 9 bits))
   in case results of
        Nothing -> error "Invalid bit"
        Just str -> str

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id
