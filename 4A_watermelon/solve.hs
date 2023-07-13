main :: IO ()
main = do
  input <- getLine
  let inputNum = read input :: Integer
  putStrLn (solve inputNum)

solve :: Integer -> String
solve i = if i > 3 && (rem i 2) == 0 then
            "YES"
          else
            "NO"
