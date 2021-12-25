readLine :: IO String
readLine = do x <- getChar
              if x == '\n' then
                return []
              else
                if x == '\DEL' then
                  do putChar '\b'
                     xs <- readLine
                     return (x:xs)
                else
                  do xs <- readLine
                     if (not (null xs)) && (head xs) == '\DEL' then
                       return (tail xs)
                     else
                       return (x:xs)


main :: IO String
main = do readLine