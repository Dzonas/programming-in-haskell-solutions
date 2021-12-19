-- Ex.1
data Nat = Zero | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult m (Succ n) = add m (mult m n)