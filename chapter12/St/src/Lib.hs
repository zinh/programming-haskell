module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) = st

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap f sta = S (\s -> let (x, s') = app sta s
                        in (f x, s'))

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x, s))
  -- <*> :: S (a -> b) -> S a -> S b
  (<*>) stf sta = S(\s -> let (f, s') = app stf s
                              (x, s'') = app sta s'
                         in (f x, s''))


instance Monad ST where
  return = pure
  -- >>= :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x, s') = app st s 
                       in app (f x) s')
