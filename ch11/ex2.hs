import TicTacToe
import System.Random

bestmove' :: Grid -> Player -> Grid
bestmove' g p = bestMoves !! randomRIO (0, length bestMoves - 1)
  where
    bestMoves = [g' | Node (g',p') _ <- ts, p' == best]
    tree = prune depth (gametree g p)
    Node (_,best) ts = minimax tree