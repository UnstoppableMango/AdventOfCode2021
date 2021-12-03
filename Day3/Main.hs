module Main where

import Text.Printf (printf)

part1 :: [String] -> Int
part1 _ = 0

part2 :: [String] -> Int
part2 _ = 0

main :: IO ()
main = do
  input <- lines <$> readFile "./input.txt"
  printf "Part1: %d" (part1 input)
  printf "Part2: %d" (part2 input)
