module Main where
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

toHeading :: (String, Int) -> (Int, Int)
toHeading ("forward", x) = (x, 0)
toHeading ("down", x) = (0, x)
toHeading ("up", x) = (0, -x)

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x, y) (u, v) = (x + u, y + v)

pairs :: [String] -> (String, Int)
pairs (x:y:_) = (x, read y)

main :: IO ()
main = do
    input <- readFile "../input.txt"
    print (uncurry (*) $ foldl add (0, 0) $ map (toHeading . pairs . words) (lines input))
