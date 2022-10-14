import Data.Bits (popCount)
import Data.Char (digitToInt)
import Data.List (transpose)

type GammaRate = Int

type EpsilonRate = Int

type PowerConsumption = Int

day3 :: IO ()
day3 = do
  input <- readFile "input.txt"
  let epsilonRate = getEpsilonRate input
      gammaRate = invertBinaryNumber <$> epsilonRate
      er = concat $ show <$> epsilonRate
      gr = concat $ show <$> gammaRate
  print er
  print gr

getPowerConsumption :: EpsilonRate -> GammaRate -> PowerConsumption
getPowerConsumption er gr = er * gr

getEpsilonRate :: String -> [EpsilonRate]
getEpsilonRate input = transformNumber <$> ones input

flattenList :: [Int] -> Int
flattenList = read . concatMap show

invertBinaryNumber :: Int -> Int
invertBinaryNumber x = if x == 1 then 0 else 1

transformNumber :: Int -> Int
transformNumber x = if x < 501 then 0 else 1

zeros :: String -> [Int]
zeros = fmap (countZeros . charsToInts) . transpose . lines

ones :: String -> [Int]
ones = fmap (countOnes . charsToInts) . transpose . lines

countZeros :: [Int] -> Int
countZeros = length . filter (0 ==)

countOnes :: [Int] -> Int
countOnes = length . filter (1 ==)

charsToInts :: String -> [Int]
charsToInts = fmap digitToInt
