instance Applicative ((->) r) where
  -- pure :: b -> (a -> b)
  pure = const

  -- (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
  g <*> h = \x -> g x (h x)