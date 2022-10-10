day2 :: IO ()
day2 = do
  input <- readFile "input.txt"
  let listOfTuples = map (wordsToPairs . words) $ lines input
      forwards = filterPairsWithCommand "forward" listOfTuples
      ups = filterPairsWithCommand "up" listOfTuples
      downs = filterPairsWithCommand "down" listOfTuples
      forwards' = sum $ map snd $ filterNullAndFlatten forwards
      ups' = sum $ map snd $ filterNullAndFlatten ups
      downs' = sum $ map snd $ filterNullAndFlatten downs
      answer = forwards' * (downs' - ups')
  print $ "Answer: " <> show answer

wordsToPairs :: [String] -> [(String, Int)]
wordsToPairs (x : y : xs) = (x, read y) : wordsToPairs xs
wordsToPairs (_ : _) = error "Error converting words to pairs"
wordsToPairs [] = []

filterPairsWithCommand :: String -> [[(String, Int)]] -> [[(String, Int)]]
filterPairsWithCommand command list = (map . filter) (\(a, _) -> a == command) list

filterNullAndFlatten :: [[(String, Int)]] -> [(String, Int)]
filterNullAndFlatten list = concat $ filter (not . null) list
