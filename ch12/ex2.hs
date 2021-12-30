instance Functor ((->) r) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap = .