module IO where

import Data.List (intercalate)
import Game (Grid, Player(..), empty, won, showPlayer, move, next)

getNat :: IO Int
getNat = do
  n <- getLine
  return (read n :: Int)

putRow :: [Player] -> IO ()
putRow = putStrLn . intercalate " | " . map showPlayer

putGrid :: Grid -> IO ()
putGrid = sequence_ . map putRow

tictactoe :: IO ()
tictactoe = run empty O

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int, Int) -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

run :: Grid -> Player -> IO ()
run gs p = do
  cls
  goto (0, 0)
  putGrid gs
  putStr ("Player " ++ (show p) ++ ": ")
  pos <- getNat
  case move gs pos p of
    [] -> do putStrLn "Invalid move. Try again"
             run gs p
    [g'] -> if won g' then
                        putStrLn (show p ++ " won")
                      else
                        run g' (next p)
