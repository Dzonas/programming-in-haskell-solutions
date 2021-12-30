data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving Show

instance Functor Expr where
  -- fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Var a) = Var (f a)
  fmap _ (Val x) = Val x
  fmap f (Add x y) = Add (fmap f x) (fmap f y)

instance Applicative Expr where
  -- pure :: a -> Expr a
  pure = Var

  -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  _ <*> Val x = Val x
  Val x <*> _ = Val x
  Var f <*> Var x = Var (f x)
  Var f <*> Add x y = Add (fmap f x) (fmap f y)
  Add f g <*> x = Add (f <*> x) (g <*> x)

instance Monad Expr where
  -- return :: a -> Expr a
  return = pure

  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  Val x >>= _ = Val x
  Var x >>= f = (f x)
  Add x y >>= f = Add (x >>= f) (y >>= f)
