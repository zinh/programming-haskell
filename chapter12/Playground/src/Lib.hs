module Lib
  ( getNumber 
  , get2Number
  , enhancedGet2Number
  , interactiveConcat
  , safeLog
  ) where

import Text.Read

getNumber :: IO ()
getNumber = do
  putStrLn "Please input a number: "
  str <- getLine
  case fmap (*2) (readMaybe str :: Maybe Int) of
    Nothing -> do
      putStrLn "Not a number, please try again"
      getNumber
    Just n -> putStrLn (show n)

get2Number :: IO ()
get2Number = do
  putStrLn "Please input 2 numbers"
  putStrLn "1st: "
  s1 <- getLine
  putStrLn "2nd: "
  s2 <- getLine
  case (readMaybe s1 :: Maybe Int) of 
    Nothing -> get2Number
    Just n1 -> case (readMaybe s2 :: Maybe Int) of
                 Nothing -> get2Number
                 Just n2 -> putStrLn (show (n1 + n2))


enhancedGet2Number :: IO ()
enhancedGet2Number = do
  putStrLn "Input 2 numbers: "
  putStrLn "1st: "
  mx <- fmap readMaybe getLine
  putStrLn "2nd: "
  my <- fmap readMaybe getLine
  case (+) <$> mx <*> my of
    Nothing -> enhancedGet2Number
    Just n -> putStrLn (show n)

interactiveConcat :: IO ()
interactiveConcat = do
  putStrLn "Choose two strings: "
  s <- (++) <$> getLine <*> getLine
  putStrLn "Concatenate of 2 strings: "
  putStrLn s


safeLog :: (Floating a, Ord a) => a -> Maybe a
safeLog x
  | x > 0 = Just (log x)
  | otherwise = Nothing
