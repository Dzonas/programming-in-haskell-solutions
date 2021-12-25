type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "*"))

putBoard :: Board -> IO ()
putBoard board = sequence_ [putRow row num | (row,num) <- zip (iterate (+1) 1) board]