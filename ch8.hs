-- Ex.1
data Nat = Zero | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)


mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult m (Succ n) = add m (mult m n)

-- Ex.2
--data Tree a = Leaf a | Node (Tree a) a (Tree a)
--occurs :: Ord a => a -> Tree a -> Bool
--occurs x (Leaf y) = x == y
--occurs x (Node l y r) = case compare x y of
--  EQ -> True
--  LT -> occurs x l
--  GT -> occurs x r

-- Ex.3
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

countLeaves :: Tree a -> Int
countLeaves (Leaf x) = 1
countLeaves (Node l r) = countLeaves l + countLeaves r

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) = abs (countLeaves l - countLeaves r) <= 1 && balanced l && balanced r

-- Ex.4
halve :: [a] -> ([a], [a])
halve xs = ((take n xs), (drop n xs))
  where n = (length xs) `div` 2

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance l) (balance r)
  where (l,r) = halve xs

-- Ex.5
--data Expr = Val Int | Add Expr Expr
--
--folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
--folde f _ (Val x) = f x
--folde f g (Add x y) = g (folde f g x) (folde f g y)

-- Ex.6
--eval :: Expr -> Int
--eval = folde id (+)

--size :: Expr -> Int
--size = folde (\_ -> 1) (+)

-- Ex.7
--instance Eq a => Eq (Maybe a) where
--  Nothing == Nothing = True
--  Just x == Just y = x == y
--  _ == _ = False
--
--
--instance Eq a => Eq [a] where
--  [] == [] = True
--  (x:xs) == (y:ys) = x == y & xs == ys
--  _ == _ = False

-- Ex.8
--type Assoc k v = [(k, v)]
--
--find :: Eq k => k -> Assoc k v -> v
--find k t = head [v | (k', v) <- t, k == k']
--
--data Prop = Const Bool
--  | Var Char
--  | Not Prop
--  | And Prop Prop
--  | Imply Prop Prop
--  | Or Prop Prop
--  | Eqv Prop Prop
--
--type Subst = Assoc Char Bool
--
--eval :: Subst -> Prop -> Bool
--eval _ (Const b) = b
--eval s (Var x) = find x s
--eval s (Not p) = not (eval s p)
--eval s (And p q) = eval s p && eval s q
--eval s (Imply p q) = eval s p <= eval s q
--eval s (Or p q) = eval s p || eval s q
--eval s (Eqv p q) = eval s p == eval s q
--
--vars :: Prop -> [Char]
--vars (Const _) = []
--vars (Var x) = [x]
--vars (Not p) = vars p
--vars (And p q) = vars p ++ vars q
--vars (Imply p q) = vars p ++ vars q
--vars (Or p q) = vars p ++ vars q
--vars (Eqv p q) = vars p ++ vars q
--
--bools :: Int -> [[Bool]]
--bools 0 = [[]]
--bools n = map (False:) bss ++ map (True:) bss
--  where bss = bools (n - 1)
--
--rmdups :: Eq a => [a] -> [a]
--rmdups [] = []
--rmdups (x:xs) = x : filter (/= x) (rmdups xs)
--
--substs :: Prop -> [Subst]
--substs p = map (zip vs) (bools (length vs))
--  where vs = rmdups (vars p)
--
--isTaut :: Prop -> Bool
--isTaut p = and [eval s p | s <- substs p]

-- Ex.9
data Expr = Val Int
  | Add Expr Expr
  | Mul Expr Expr

type Cont = [Op]
data Op = EVAL_ADD Expr
  | ADD Int
  | EVAL_MUL Expr
  | MUL Int

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL_ADD y : c)
eval (Mul x y) c = eval x (EVAL_MUL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL_ADD y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (n+m)
exec (EVAL_MUL y : c) n = eval y (MUL n : c)
exec (MUL n : c) m = exec c (n*m)

value :: Expr -> Int
value e = eval e []