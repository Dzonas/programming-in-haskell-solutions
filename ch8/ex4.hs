data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

halve :: [a] -> ([a], [a])
halve xs = ((take n xs), (drop n xs))
  where n = (length xs) `div` 2

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance l) (balance r)
  where (l,r) = halve xs