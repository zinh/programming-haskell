module Ex where

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
  pure a = Z (repeat a)
  (<*>) (Z fs) (Z xs) = Z [f x | (f, x) <- zip fs xs]

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving Show

instance Functor Expr where
  -- fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Var a) =  Var (f a)

instance Applicative Expr where
  pure = error "not implemented"
  (<*>) = error "not implemented"

instance Monad Expr where
  (<<=) = error "not implemented"
