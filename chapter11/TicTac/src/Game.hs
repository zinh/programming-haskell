module Game where

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
wins player gs = win' gs || win' (transpose gs)
  where rowFilled = all (== player)
        win' gss = any rowFilled gss || rowFilled (diag gss)

diag :: Grid -> [Player]
diag gs = [row !! idx | (row, idx) <- zip gs [0..]]

transpose :: Grid -> Grid
transpose [] = []
transpose gs@(g:_) = foldr appendRow (replicate (length g) []) gs
  where appendRow (cell:row) (r:rs) = (r ++ [cell]) : (appendRow row rs)
        appendRow [] memo = memo

won :: Grid -> Bool
won gs = wins X gs || wins O gs


showPlayer :: Player -> String
showPlayer B = " "
showPlayer p = show p

valid :: Grid -> Int -> Bool
valid gs i =
  let rowNo = i `div` size
      colNo = i `mod` size
   in rowNo < size && (gs !! rowNo !! colNo == B)

move :: Grid -> Int -> Player -> [Grid]
move gs pos p
  | valid gs pos = [makeMove gs pos p]
  | otherwise = []

-- make a move
makeMove :: Grid -> Int -> Player -> Grid
makeMove gs pos p =
  let rowNo = pos `div` size
      colNo = pos `mod` size
      row = gs !! rowNo
      updatedRow = (take colNo row) ++ [p] ++ (drop (colNo + 1) row)
   in (take rowNo gs) ++ [updatedRow] ++ (drop (rowNo + 1) gs)

