import TicTacToe
import System.IO

getPlayer :: String -> IO Player
getPlayer prompt = do putStr prompt
                      xs <- getLine
                      case xs of
                        "O" -> return O
                        "X" -> return X
                        _   -> do putStrLn "ERROR: Invalid player"
                                  getPlayer prompt

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          player <- getPlayer "Pick first player (X or O): "
          play empty player