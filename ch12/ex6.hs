instance Applicative ((->) r) where
  -- pure :: b -> (a -> b)
  pure = const

  -- (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
  g <*> h = \x -> g x (h x)

instance Monad ((->) r) where
  -- return :: a -> (r -> a)
  return  = const

  -- (>>=) :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
  (>>=) f g r = g (f r) r
