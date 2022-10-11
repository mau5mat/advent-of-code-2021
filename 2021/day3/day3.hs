import Text.Read (readMaybe)
-- Modelling data
type GammaRate = Int

type EpsilonRate = Int

type PowerConsumption = Int

day3 :: IO ()
day3 = do
  input <- readFile "input.txt"
  print $ parseInput input

parseInput :: String -> [[Int]]
parseInput input = stringsToInts $ words <$> lines input

stringsToInts :: [[String]] -> [[Int]]
stringsToInts =  (fmap . fmap) read



-- Parse input data from String -> [[Int]]
-- [[1,1,0],[0,1,0],[0,0,1]] -- Start
-- --> [[1,0,0],[1,1,0],[0,0,1]] -- New lists from positions of the elements in lists
-- --> --> [[0],[1],[0]] --> Return 0 or 1 depending on highest precident
-- --> --> --> 010 --> Flatten to Int
-- --> --> --> --> 2 --> Convert Int to Binary representation == GammaRate
