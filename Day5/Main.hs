import Text.Printf (printf)
import Data.Char (digitToInt)
import Control.Applicative (liftA2)

data Point = Point { x, y :: Int }
  deriving (Eq, Show)

data Line = Line { start, end :: Point }
  deriving (Show)

type Input = [Line]

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = take n xs : partition n (drop n xs)

showGrid :: [Int] -> String
showGrid = unlines . map show . partition 10

xEqual :: Line -> Bool
xEqual (Line start end) = x start == x end

yEqual :: Line -> Bool
yEqual (Line start end) = y start == y end

toRange :: Point -> Point -> [Point]
toRange start end = Point <$> spread x <*> spread y
  where spread getDim
          | getDim start < getDim end = [(getDim start)..(getDim end)]
          | otherwise = [(getDim end)..(getDim start)]

grid :: [Line] -> [Point]
grid = generate . maxPoints . squash
  where generate = toRange (Point 0 0)
        maxPoints l = Point (maxX l) (maxY l)
        maxX = maximum . map x
        maxY = maximum . map y
        squash [] = []
        squash ((Line start end):xs) = start:end:squash xs

covers :: Line -> Point -> Bool
covers (Line start end) point = elem point $ toRange start end

countCovers :: Line -> [Point] -> [Int]
countCovers l = map (toInt . covers l)
  where toInt True = 1
        toInt False = 0

counts :: [Line] -> [Point] -> [Int]
counts lines grid = foldl go (replicate (length grid) 0) lines
  where go acc l = zipWith (+) acc $ countCovers l grid        

part1 :: Input -> Int
part1 input = length $ filter (>=2) $ counts hv $ grid hv
  where hv = filter (liftA2 (||) xEqual yEqual) input

part2 :: Input -> Int
part2 _ = 0

prepare :: [String] -> Input
prepare = map toLine
  where toLine i = Line {
          start = toPoint $ head $ words i,
          end = toPoint $ last $ words i
        }
        toPoint csv = Point {
          x = digitToInt $ head csv,
          y = digitToInt $ last csv
        }

main :: IO ()
main = do
  input <- lines <$> readFile "./test.txt"
  printf "Part1: %d\n" (part1 $ prepare input)
  printf "Part2: %d\n" (part2 $ prepare input)
