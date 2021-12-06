module Main where

import Data.Char (digitToInt)
import Text.Printf (printf)
import Data.Map (mapMaybe)

type Input = ([Int], [Board])

data Tile = Marked | Unmarked Int deriving Show

type Board = [[Tile]]

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitOn c s''
    where
      (w, s'') = break (== c) s'

toTile :: String -> Tile
toTile = Unmarked . read

part1 :: Input -> Int
part1 _ = 0

part2 :: Input -> Int
part2 _ = 0

toBoard :: [String] -> Board
toBoard = map (map toTile . words)

prepare :: [String] -> Input
prepare x = (toRolls $ head x, toBoards $ tail x)
  where
    toRolls csv = map read $ splitOn ',' csv
    toBoards l = map toBoard $ filter (/= []) $ transform l
    transform [] = []
    transform l = take 5 $ tail l : transform (drop 5 $ tail l)

main :: IO ()
main = do
  input <- lines <$> readFile "./input.txt"
  printf "Part1: %d\n" (part1 $ prepare input)
  printf "Part2: %d\n" (part2 $ prepare input)
