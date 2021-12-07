import Text.Printf (printf)
import Data.Char (digitToInt)
import Control.Applicative (liftA2)

data Point = Point { x, y :: Int }
  deriving (Eq, Show)

data Line = Line { start, end :: Point }
  deriving (Show)

type Input = [Line]

xEqual :: Line -> Bool
xEqual (Line start end) = x start == x end

yEqual :: Line -> Bool
yEqual (Line start end) = y start == y end

toRange :: Point -> Point -> [Point]
toRange start end = map (uncurry Point) $ (,) <$> [(x start)..(x end)] <*> [(y start)..(y end)]

getGrid :: [Line] -> [Point]
getGrid = generate . maxPoints . squash
  where generate = toRange (Point 0 0)
        maxPoints l = Point (maxX l) (maxY l)
        maxX = maximum . map (\(Point x y) -> x)
        maxY = maximum . map (\(Point x y) -> y)
        squash [] = []
        squash ((Line start end):xs) = start:end:squash xs

covers :: Point -> Line -> Bool
covers point (Line start end) = elem point $ toRange start end

part1 :: Input -> Int
part1 input = 0
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
