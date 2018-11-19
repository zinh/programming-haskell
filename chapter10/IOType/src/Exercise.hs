module Exercise where

adder :: IO ()
adder = do
  putStrLn "How many numbers?"
  total <-  fmap (read::String -> Int) getLine
  lst <- fmap sumString $ sequence [getLine | t <- [1..total]]
  putStrLn ("The total is: " ++ (show lst))

sumString :: [String] -> Int
sumString arr = sum [read a :: Int | a <- arr]

readLine' :: IO String
readLine' = do
  c <- getChar
  case c of 
    '\n' -> return ""
    '\DEL' -> do
      putChar '\b'
      readLine'
    otherwise -> do
      rest <- readLine'
      return (c:rest)
