module Game where

import Data.List (intercalate)

data Player = O | B | X deriving (Eq, Show, Ord)

type Grid = [[Player]]

size = 3 :: Int

next :: Player -> Player
next O = X
next X = O
next B = B

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/=B) . concat

turn :: Grid -> Player
turn g = if countX > countO then O else X
  where gs = concat g
        countX = length (filter (==X) gs)
        countO = length (filter (==O) gs)

wins :: Player -> Grid -> Bool
wins = error "not implemented"

diag :: Grid -> [Player]
diag gs = [row !! idx | (row, idx) <- zip gs [0..]]

won :: Grid -> Player
won gs = wins O gs || wins X gs

putGrid :: Grid -> IO ()
putGrid = sequence_ . map putRow

putRow :: [Player] -> IO ()
putRow = putStrLn . intercalate " | " . map showPlayer

showPlayer :: Player -> String
showPlayer B = " "
showPlayer p = show p

valid :: Grid -> Int -> Bool
valid gs i =
  let rowNo = i `div` size
      colNo = i `mod` size
   in gs !! rowNo !! colNo == B

move :: Grid -> Int -> Player -> [Grid]
move gs pos p
  | valid gs pos = [makeMove gs pos p]
  | otherwise = []

-- make a move
makeMove :: Grid -> Int -> Player -> Grid
makeMove = error "not implemented"

-- getNat
getNat :: IO Int
getNat = error "not implemented"

tictactoe :: IO ()
tictactoe = error "not implemented"

run :: Grid -> Player -> IO ()
run = error "not implemented"
