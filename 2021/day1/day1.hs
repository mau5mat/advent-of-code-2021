day1 :: IO ()
day1 = do
  input <- readFile "input.txt"

  let fileLines = lines input
  let list = getIntList fileLines
  let sumOfTuples = parseInput' list

  let answer1a = parseInput list
  let answer1b = parseInput sumOfTuples

  print $ length answer1a
  print $ length answer1b

getIntList :: [String] -> [Int]
getIntList = map read

parseInput :: [Int] -> [(Int, Int)]
parseInput [] = []
parseInput (x : y : xs) | x < y = (x, y) : parseInput (y : xs)
parseInput (x : xs) = parseInput xs

parseInput' :: [Int] -> [Int]
parseInput' [] = []
parseInput' (x : y : z : xs) = (x + y + z) : parseInput' (y : z : xs)
parseInput' _ = []
