import TicTacToe

countNodes :: Tree a -> Int
countNodes (Node _ []) = 1
countNodes (Node _ ts) = 1 + sum (map countNodes ts)

main :: IO ()
main = do putStrLn (show (countNodes (prune depth (gametree empty O))))