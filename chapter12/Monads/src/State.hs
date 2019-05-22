module State where

type State = Int

newtype ST a = S (State -> (State, a))

app :: ST a -> State -> (State, a)
app (S t) = t

instance Functor ST where
  --fmap :: (a -> b) -> ST a -> ST b
  fmap f st = S (\s -> let (s', a) = app st s in (s', f a))

instance Applicative ST where
  pure a = S (\s -> (s, a))
  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  (<*>) stf stx = S (\s -> 
    let (s', f) = app stf s
        (s'', x) = app stx s'
     in (s'', f x))

instance Monad ST where
  -- >>= :: ST a -> (a -> ST b) -> ST b
  (>>=) stx f = S (\s -> let (s', a) = app stx s in app (f a) s')
