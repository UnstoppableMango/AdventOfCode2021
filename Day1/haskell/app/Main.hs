module Main where
import Text.Read (readMaybe)
import Data.Maybe (catMaybes, mapMaybe)

increases :: Int -> Int -> Int
increases x y
    | x < y = 1
    | otherwise = 0

tupleIncreases :: (Int, Int) -> Int
tupleIncreases (x, y) = increases x y

getValues :: [String] -> [Int]
getValues = mapMaybe readMaybe

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [x, y] = [(x, y)]
pairs (x:y:xs) = (x, y) : pairs (y : xs)

main :: IO ()
main = do
    input <- readFile "../input.txt"
    let values = getValues $ lines input
    print $ sum $ map tupleIncreases $ pairs values
