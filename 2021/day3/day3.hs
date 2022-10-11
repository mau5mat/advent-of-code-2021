day3 :: IO ()
day3 = undefined

-- Parse input data from String -> [[Int]]
-- [[1,1,0],[0,1,0],[0,0,1]] -- Start
-- --> [[1,0,0],[1,1,0],[0,0,1]] -- New lists from positions of the elements in lists
-- --> --> [[0],[1],[0]] --> Return 0 or 1 depending on highest precident
-- --> --> --> 010 --> Flatten to Int
-- --> --> --> --> 2 --> Convert Int to Binary representation
