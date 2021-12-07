import Text.Printf (printf)

type Input = [String]

part1 :: Input -> Int
part1 _ = 0

part2 :: Input -> Int
part2 _ = 0

prepare :: [String] -> Input
prepare x = x

main :: IO ()
main = do
  input <- lines <$> readFile "./input.txt"
  printf "Part1: %d\n" (part1 $ prepare input)
  printf "Part2: %d\n" (part2 $ prepare input)
