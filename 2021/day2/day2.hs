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
      firstAnswer = pt1Answer forwards' downs' ups'
      secondAnswer = pt2Answer forwards' downs' ups'
  print $ "Pt1 Answer: " <> show firstAnswer
  print $ "Pt2 Answer: " <> show secondAnswer

wordsToPairs :: [String] -> [(String, Int)]
wordsToPairs (x : y : xs) = (x, read y) : wordsToPairs xs
wordsToPairs (_ : _) = error "Error converting words to pairs"
wordsToPairs [] = []

filterPairsWithCommand :: String -> [[(String, Int)]] -> [[(String, Int)]]
filterPairsWithCommand command = (map . filter) (\(a, _) -> a == command)

filterNullAndFlatten :: [[(String, Int)]] -> [(String, Int)]
filterNullAndFlatten list = concat $ filter (not . null) list

pt1Answer :: Int -> Int -> Int -> Int
pt1Answer forwards downs ups = forwards * (downs - ups)

pt2Answer :: Int -> Int -> Int -> Int
pt2Answer forward downs ups = aim * depth
  where
    aim = downs - ups
    depth = forward * aim

-- pt2

-- Total Aim = Down - Up
-- Depth d = Aim * d
-- Answer = total Horizontal Position x total Depth
