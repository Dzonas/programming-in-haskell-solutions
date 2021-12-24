rmval :: Eq a => a -> [a] -> [a]
rmval _ [] = []
rmval v (x:xs)
  | v == x = xs
  | otherwise = x : rmval v xs

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x:xs) ys
  | x `elem` ys = isChoice xs (rmval x ys)
  | otherwise = False