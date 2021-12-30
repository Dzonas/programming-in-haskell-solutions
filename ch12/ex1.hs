data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> f a -> f b
  fmap _ Leaf = Leaf
  fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)