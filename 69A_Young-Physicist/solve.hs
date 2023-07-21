main :: IO ()
main = do
  input <- getLine
  let count = read input :: Int
  let ioVectors = consume count (\() -> parseVector <$> getLine)
  let ioAccumulated = foldl1 (\ioAcc ioVector -> (addVector <$> ioAcc) <*> ioVector) ioVectors
  accumulated <- ioAccumulated
  putStrLn $ if checkZeroVector accumulated then "YES" else "NO"

consume :: Int -> (() -> b) -> [b]
consume 0 _ = []
consume n f = f() : consume (n - 1) f

data Vector = Vector Int Int Int deriving Show
parseVector :: String -> Vector
parseVector input =
  let
    splittedString = words input
    splitted = map (\elem -> read elem :: Int) splittedString
  in
  Vector (splitted !! 0) (splitted !! 1) (splitted !! 2)

addVector :: Vector -> Vector -> Vector
addVector (Vector aa ab ac) (Vector ba bb bc) = Vector (aa + ba) (ab + bb) (ac + bc)

checkZeroVector :: Vector -> Bool
checkZeroVector (Vector a b c) = a == 0 && b == 0 && c == 0
