import Data.Char (digitToInt)

-- Modelling data
type GammaRate = Int

type EpsilonRate = Int

type PowerConsumption = Int

day3 :: IO ()
day3 = do
  input <- readFile "input.txt"
  print $ parseInput input

parseInput :: String -> [[Int]]
parseInput = charsToInts . lines

f :: [Int] -> [Int] -> [[Int]]
f (x : xs) (y : ys) = [x, y] : f xs ys
f [] [] = []
f [_] [] = []
f [] [_] = []
f (x : xs) [] = []
f [] (y : xs) = []

charsToInts :: [String] -> [[Int]]
charsToInts = (fmap . fmap) digitToInt

countZeros :: [[Int]] -> [Int]
countZeros = fmap (length . filter (0 ==))

countOnes :: [[Int]] -> [Int]
countOnes = fmap (length . filter (1 ==))

-- Parse input data from String -> [[Int]]
-- [[1,1,0],[0,1,0],[0,0,1]] -- Start
-- --> [[1,0,0],[1,1,0],[0,0,1]] -- New lists from positions of the elements in lists
-- --> --> [[0],[1],[0]] --> Return 0 or 1 depending on highest precident
-- --> --> --> 010 --> Flatten to Int
-- --> --> --> --> 2 --> Convert Int to Binary representation == GammaRate
