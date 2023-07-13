import Data.Functor

main :: IO ()
main = do
  input <- getLine
  let inputNum = read input :: Integer
  consume inputNum >>= print

consume :: Integer -> IO Integer
consume 0 = return 0
consume count = getLine >>= (\line -> 
          consume (count - 1) >>= (\prev -> 
            return (solve (parse line) + prev)
          )
        )

parse :: String -> (Integer, Integer, Integer)
parse input = tuplify3 $ map (\elem -> read elem :: Integer) $ words input

tuplify3 :: [Integer] -> (Integer, Integer, Integer)
tuplify3 [a, b, c] = (a, b, c)

solve :: (Integer, Integer, Integer) -> Integer
solve (a, b, c) = if a + b + c > 1 then 1 else 0
