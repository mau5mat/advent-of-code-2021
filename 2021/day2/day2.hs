-- Foward, Up, Down from [String]
-- Total Down - Total Up = Total Current Depth
-- Total Forward x Total Current Depth = Answer

-- Take every array that contains Forward and make a new array, 
-- flatten it, remove words
-- convert remainding string int to int and sum

main :: IO ()
main = do
    input <- readFile "input.txt"
    let listOfTuples = map (wordsToPairs . words) $ lines input
        forwards = filterPairsWithCommand "forward" listOfTuples
        ups = filterPairsWithCommand "up" listOfTuples
        downs = filterPairsWithCommand "down" listOfTuples
    print $ concat $ filter (not . null) ups

wordsToPairs :: [String] -> [(String, Int)]
wordsToPairs (x:y:xs) = (x, read y) : wordsToPairs xs
wordsToPairs (_:_) = error "number of words not divisible by 2"
wordsToPairs [] = []

total :: [[(String, Int)]] -> Int
total list = undefined

filterPairsWithCommand :: String -> [[(String, Int)]] -> [[(String, Int)]]
filterPairsWithCommand command list = (map . filter) (\(a, _) -> a == command) list
