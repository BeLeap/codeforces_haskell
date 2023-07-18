import Data.Char

main :: IO()
main = do
  line <- getLine
  let (_, sum) = solve line
  if sum >= 7 then
    putStrLn "YES"
  else
    putStrLn "NO"

solve :: String -> (Maybe Int, Int)
solve "" = (Nothing, 0)
solve (x:xs) =
  let
    xInt = digitToInt x
    (next, sum) = solve xs
  in
  case next of
    Just next -> (Just xInt, if next == xInt then 1 + sum else if sum >= 7 then sum else 1)
    Nothing -> (Just xInt, 1)
  
