import Data.Functor

main :: IO ()
main = do
  input <- getLine
  let inputNum = read input :: Integer
  foldr1 (>>) (map (>>= putStrLn) (consume inputNum))

consume :: Integer -> [IO String]
consume 0 = []
consume num = (getLine <&> solve) : consume (num - 1)

solve :: String -> String
solve str = if length str > 10 then [head str] ++ show (length str - 2) ++ [last str] else str
