-- Ex.1
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
            where
                n = (length xs) `div` 2

-- Ex.2
third1 :: [a] -> a
third1 xs = head (tail (tail xs))

third2 :: [a] -> a
third2 xs = xs !! 2

third3 :: [a] -> a
third3 (_:_:x:_) = x

-- Ex.3
safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

safetail2 :: [a] -> [a]
safetail2 xs | null xs = []
             | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs

-- Ex.4
{-
(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False

(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True

(||) :: Bool -> Bool -> Bool
a || True = a
_ || False = False
-}

-- Ex.5
{-
(&&) :: Bool -> Bool -> Bool
(&&) x y = if x then if y then True else False else False
-}

-- Ex.6
{-
(&&) :: Bool -> Bool -> Bool
(&&) x y = if x then if y then y else False else False
-}

-- Ex.7
mult :: Int -> (Int -> (Int -> Int))
mult = \x -> (\y -> (\z -> x * y * z))

-- Ex.8
luhnDouble :: Int -> Int
luhnDouble x | n > 9 = n - 9
             | otherwise = n
               where n = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d | s `mod` 10 == 0 = True
             | otherwise = False
	       where a_doubled = luhnDouble a
	             c_doubled = luhnDouble c
		     s = a_doubled + b + c_doubled + d
