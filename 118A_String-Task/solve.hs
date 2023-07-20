import Data.Char

main :: IO ()
main = do
  input <- getLine
  putStrLn $ solve input

solve :: String -> String
solve input = foldl (\acc elem -> acc ++ "." ++ [elem]) "" $ foldl removeVowel "" $ map toLower input

removeVowel :: String -> Char -> String
removeVowel acc elem = acc ++ if isVowel elem then "" else [elem]

isVowel :: Char -> Bool
isVowel input | input `elem` "aeiouy" = True
              | otherwise = False
