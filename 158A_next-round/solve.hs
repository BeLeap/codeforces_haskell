main :: IO ()
main = do
  input <- getLine
  let condition = parseCondition input

  case condition of
    Just (Condition _ cut) -> do
      scores <- getLine
      let parsedScores = parseScores scores
      print (solve (parsedScores !! fromIntegral (cut - 1)) parsedScores)
    _ -> error "InvalidCondition"

data Condition = Condition { all :: Integer, cut :: Integer }

parseCondition :: String -> Maybe Condition
parseCondition input = case words input of 
                        [all, cut] -> Just $ Condition (read all :: Integer) (read cut :: Integer)
                        _ -> Nothing

parseScores :: String -> [Integer]
parseScores input = map (\elem -> (read elem :: Integer)) $ words input

solve :: Integer -> [Integer] -> Integer
solve cut = foldl (\acc curr -> if curr >= cut && curr > 0 then acc + 1 else acc) 0
