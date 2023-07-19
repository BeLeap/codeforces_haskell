main :: IO ()
main = do
  line <- getLine
  print $ solve $ parse line

data Input = Input {
  n :: Integer,
  m :: Integer,
  a :: Integer
}
parse :: String -> Input
parse input = 
  let
    splitted = map (\elem -> read elem :: Integer) (words input)
  in
  Input {
    n = splitted !! 0,
    m = splitted !! 1,
    a = splitted !! 2
  }

solve :: Input -> Integer
solve Input { n=n, m=m, a=a } = divide n a * divide m a

divide :: Integer -> Integer -> Integer
divide a b = if mod a b == 0 then div a b else div a b + 1

