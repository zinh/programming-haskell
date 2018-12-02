module Life where
  
import IO (cls)
import Control.Concurrent (threadDelay)

-- position on screen, top-left is (0, 0)
type Pos = (Int, Int)
type Board = [Pos]

width :: Int
width = 10

height :: Int
height = 10

glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

isAlive :: Board -> Pos -> Bool
isAlive board p = p `elem` board

nextState :: Board -> Board
nextState board = [(x, y) | x <- [0..(width - 1)], y <- [0..(height - 1)], survive board (x, y)]

survive :: Board -> Pos -> Bool
survive board pos@(x, y) = 
  let neighborCount = sum [1 | neighbor <- neighborCells pos, isAlive board neighbor]
   in if isAlive board pos then neighborCount >= 2 && neighborCount <= 3
      else neighborCount == 3
          
neighborCells :: Pos -> [Pos]
neighborCells (x, y) = [(x + i, y + j) | i <- [0, 1, -1], 
    j <- [0, 1, -1], 
    x + i >= 0 && x + i < width, 
    y + j >= 0 && y + j < height, 
    i /= 0 || j /= 0]

writeat :: Pos -> String -> IO ()
writeat p text = do goto p
                    putStr text

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

showcells :: Board -> IO ()
showcells board = sequence_ [if (x, y) `elem` board then writeat (x, y) "O" else writeat (x, y) "." | x <- [0..(width - 1)], y <- [0..(height -1)]]

run :: Int -> Int -> Board -> IO ()
run loopCount delay board = 
  if loopCount == 0 then return () 
                    else
                      do
                        showcells board
                        threadDelay delay
                        run (loopCount - 1) delay (nextState board)
