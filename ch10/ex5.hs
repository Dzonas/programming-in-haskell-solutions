getNumber :: IO Int
getNumber = do line <- getLine
               return (read line :: Int)

adder :: IO ()
adder = do putStr "How many numbers? "
           n <- getNumber
           ns <- sequence (take n (repeat getNumber))
           putStr "The total is "
           putStrLn (show (sum ns))