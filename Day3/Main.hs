module Main where

import Text.Printf (printf)

part1 :: [String] -> Int
part1 input = 0

part2 :: [String] -> Int
part2 _ = 0

main :: IO ()
main = do
  input <- lines <$> readFile "./test.txt"
  printf "Part1: %d\n" (part1 input)
  printf "Part2: %d\n" (part2 input)
