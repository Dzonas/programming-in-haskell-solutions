-- Ex.1
fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n-1)

-- Ex.2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- Ex.3
my_exp :: Int -> Int -> Int
my_exp _ 0 = 1
my_exp a b = a * my_exp a (b-1)

-- Ex.4
euclid :: Int -> Int -> Int
euclid a b | a == b = a
           | otherwise = euclid lesser (greater - lesser)
           where greater = max a b
                 lesser = min a b

-- Ex.6
my_and :: [Bool] -> Bool
my_and [] = True
my_and (x:xs) = x && my_and xs

my_concat :: [[a]] -> [a]
my_concat [] = []
my_concat (x:xss) = x ++ my_concat xss

my_replicate :: Int -> a -> [a]
my_replicate 0 _ = []
my_replicate n x = x : replicate (n-1) x

(!!) :: [a] -> Int -> a
(!!) (x:_) 0 = x
(!!) (_:xs) n = xs Main.!! (n-1)

my_elem :: Eq a => a -> [a] -> Bool
my_elem _ [] = False
my_elem y (x:xs) | y == x = True
                 | otherwise = my_elem y xs

-- Ex.7
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | x > y = y : merge (x:xs) ys

-- Ex.8
halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
           where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort xs | length xs == 1 = xs
         | otherwise = merge (msort a) (msort b)
                       where (a,b) = halve xs

-- Ex.9
my_sum :: Num a => [a] -> a
my_sum [] = 0
my_sum (x:xs) = x + my_sum xs

my_take :: Int -> [a] -> [a]
my_take 0 _ = []
my_take _ [] = []
my_take n (x:xs) = x : my_take (n-1) xs

my_last :: [a] -> a
my_last [x] = x
my_last (_:xs) = my_last xs
