data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

countLeaves :: Tree a -> Int
countLeaves (Leaf x) = 1
countLeaves (Node l r) = countLeaves l + countLeaves r

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) = abs (countLeaves l - countLeaves r) <= 1 && balanced l && balanced r