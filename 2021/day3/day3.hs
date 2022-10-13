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
      gammaRate = getGammaRate input
  print epsilonRate
  print gammaRate

getPowerConsumption :: EpsilonRate -> GammaRate -> PowerConsumption
getPowerConsumption er gr = er * gr

getEpsilonRate :: String -> EpsilonRate
getEpsilonRate input = flattenList $ transformNumber <$> ones input

getGammaRate :: String -> GammaRate
getGammaRate input = flattenList $ transformNumber <$> zeros input

flattenList :: [Int] -> Int
flattenList = read . concatMap show

transformNumber :: Int -> Int
transformNumber x = if x < 500 then 1 else 0

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
