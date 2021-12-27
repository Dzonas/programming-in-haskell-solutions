import TicTacToe
import Data.List
import Data.Function
import System.IO

treeDepth :: Tree a -> Int
treeDepth (Node _ []) = 1
treeDepth (Node _ ts) = 1 + maximum (map treeDepth ts)

bestmove' :: Grid -> Player -> Grid
bestmove' g p = snd (minimumBy (compare `on` fst) possibleMoves)
               where
                  tree = prune depth (gametree g p)
                  Node (_,best) ts = minimax tree
                  possibleMoves = [(treeDepth (Node (g',p') t), g') | Node (g',p') t <- ts, p' == best]

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1,1)
              putGrid g
              Main.play' g p

play' :: Grid -> Player -> IO ()
play' g p
   | wins O g = putStrLn "Player O wins!\n"
   | wins X g = putStrLn "Player X wins!\n"
   | full g   = putStrLn "It's a draw!\n"
   | p == O   = do i <- getNat (prompt p)
                   case move g i p of
                      []   -> do putStrLn "ERROR: Invalid move"
                                 Main.play' g p
                      [g'] -> Main.play g' (next p)
   | p == X   = do putStr "Player X is thinking... "
                   (Main.play $! (bestmove' g p)) (next p)

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          Main.play empty O