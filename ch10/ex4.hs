getNumber :: IO Int
getNumber = do line <- getLine
               return (read line :: Int)

getNumbers :: Int -> IO [Int]
getNumbers 0 = do return []
getNumbers n = do x <- getNumber
                  xs <- getNumbers (n - 1)
                  return (x:xs)

adder :: IO ()
adder = do putStr "How many numbers? "
           n <- getNumber
           ns <- getNumbers n
           putStr "The total is "
           putStrLn (show (sum ns))