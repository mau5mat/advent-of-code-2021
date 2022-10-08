main :: IO ()
main = do
    input <- readFile "input.txt"

    let fileLines = lines input
    let list = getIntList fileLines
    let sumOfTuples = parseInput' list

    let answer1a = parseInput list
    let answer1b = parseInput sumOfTuples

    putStrLn $ show $ length $ answer1a
    putStrLn $ show $ length $ answer1b

getIntList :: [String] -> [Int]
getIntList xs = map read xs

parseInput :: [Int] -> [(Int, Int)]
parseInput [] = []
parseInput (x: y: xs) | x < y = (x, y) : parseInput (y: xs)
parseInput (x: xs) = parseInput xs

parseInput' :: [Int] -> [Int]
parseInput' [] = []
parseInput' (x: y: z: xs) = (x + y + z) : parseInput' (y: z: xs)
parseInput' _ = []