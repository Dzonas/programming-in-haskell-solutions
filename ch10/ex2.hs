type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "*"))

putAux :: Int -> Board -> IO ()
putAux _ [] = do return ()
putAux n board = do putRow (n+1) (head board)
                    putAux (n+1) (tail board)

putBoard :: Board -> IO ()
putBoard board = putAux 0 board