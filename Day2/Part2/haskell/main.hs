module Main where
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

data Action = Forward Int | Aim Int

pairs :: [String] -> (String, Int)
pairs (x:y:_) = (x, read y)

toAction :: (String, Int) -> Action
toAction ("forward", x) = Forward x
toAction ("down", x) = Aim x
toAction ("up", x) = Aim (-x)

toHeading :: (Int, Int, Int) -> Action -> (Int, Int, Int)
toHeading (x, y, aim) (Forward amm) = (x + amm, y + (amm * aim), aim)
toHeading (x, y, aim) (Aim amm) = (x, y, aim + amm)

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x, y) (u, v) = (x + u, y + v)

main :: IO ()
main = do
    input <- readFile "../test.txt"
    print ((\(x, y, _) -> x * y) $ foldl toHeading (0, 0, 0) $ map (toAction . pairs . words) (lines input))
