import Data.Char
-- Ex.1
sum_of_squares = sum [x^2 | x <- [1..100]]

-- Ex.2
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- Ex.3
square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

-- Ex.4
replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..n]]

-- Ex.5
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2+y^2 == z^2]

-- Ex.6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) - x == x]

-- Ex.7
lc1 = [(x,y) | x <- [1,2], y <- [3,4]]
lc2 = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

-- Ex.8
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

-- Ex.9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]

-- Ex.10

let2int :: Char -> Int
let2int c | isLower c = ord c - ord 'a'
          | otherwise = ord c - ord 'A'

int2let :: Int -> Bool -> Char
int2let n is_lower | is_lower = chr (ord 'a' + n)
                   | otherwise = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c || isUpper c = int2let ((let2int c + n) `mod` 26) (isLower c)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
