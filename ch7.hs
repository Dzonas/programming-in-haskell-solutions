import Data.Char

-- Ex.1
-- map f (filter p xs)

-- Ex.2
my_all :: (a -> Bool) -> [a] -> Bool
my_all f = and . map f

my_any :: (a -> Bool) -> [a] -> Bool
my_any f = or . map f

my_takeWhile :: (a -> Bool) -> [a] -> [a]
my_takeWhile _ [] = []
my_takeWhile f (x:xs) | f x = x : my_takeWhile f xs
                      | otherwise = []

my_dropWhile :: (a -> Bool) -> [a] -> [a]
my_dropWhile _ [] = []
my_dropWhile f (x:xs) | f x = my_dropWhile f xs
                      | otherwise = x:xs 

-- Ex.3
my_map f = foldr (\x acc -> f x : acc) []
my_filter f = foldr (\x acc -> if f x then x:acc else acc) []

-- Ex.4
dec2int :: [Int] -> Int
dec2int = foldl (\acc n -> acc*10 + n) 0

-- Ex.5
my_curry :: ((a, b) -> c) -> a -> b -> c
my_curry f = \x y -> f (x, y)

my_uncurry :: (a -> b -> c) -> (a, b) -> c
my_uncurry f = \(x, y) -> f x y

-- Ex.6
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

chop8 :: [a] -> [[a]]
chop8 = unfold null (take 8) (drop 8)

unfold_map :: (a -> b) -> [a] -> [b]
unfold_map f = unfold null (f . head) tail

unfold_iterate :: (a -> a) -> a -> [a]
unfold_iterate f = unfold (\_ -> False) id f

-- Ex.7
type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

xor :: Bit -> Bit -> Bit
xor 0 0 = 0
xor 0 1 = 1
xor 1 0 = 1
xor 1 1 = 0

parity :: [Bit] -> Bit
parity bs = foldl xor 0 bs

add_parity :: [Bit] -> [Bit]
add_parity bs = (parity bs) : bs

check_parity :: [Bit] -> [Bit]
check_parity bs 
  | parity_bit == parity content = content
  | otherwise = error "parity check failed"
  where parity_bit = head bs
        content = tail bs

chop9 :: [a] -> [[a]]
chop9 = unfold null (take 9) (drop 9)

encode :: String -> [Bit]
encode = concat . map (add_parity . make8 . int2bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int . check_parity) . chop9

transmit :: String -> String
transmit = decode . channel . encode

-- Ex.8
channel :: [Bit] -> [Bit]
channel = tail

-- Ex.9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g xs = f (head xs) : altMap g f (tail xs)

-- Ex.10
luhnDouble :: Int -> Int
luhnDouble n
  | x > 9 = x - 9
  | otherwise = x
  where x = n * 2

luhn :: [Int] -> Bool
luhn ns = sum (altMap id luhnDouble (reverse ns)) `mod` 10 == 0
