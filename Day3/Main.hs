module Main where

import Data.Char (digitToInt, intToDigit)
import Data.List (foldl')
import Text.Printf (printf)
import Data.Maybe (mapMaybe)
import GHC.Float

toDecimal :: String -> Int
toDecimal = foldl' (\acc x -> acc * 2 + digitToInt x) 0

add :: Char -> Char -> Int
add a b = digitToInt a + digitToInt b

toMostLeastCommon :: Float -> Float -> Maybe (Char, Char)
toMostLeastCommon c l
  | c > l = Just ('1', '0')
  | c < l = Just ('0', '1')
  | c == l = Nothing

concatTuple :: [(Char, Char)] -> (String, String)
concatTuple = foldr (\(a, b) (x, y) -> (a:x, b:y)) ("", "")

getMostLeastCommon :: [String] -> (String, String)
getMostLeastCommon input =
  concatTuple
  $ mapMaybe (\x -> toMostLeastCommon (int2Float x) (int2Float (length input) / 2))
  $ foldl calc (replicate (length $ head input) 0) input

gmmlcAcc :: Maybe (Char, Char) -> (String, String) -> (String, String)
gmmlcAcc (Just (x, y)) (a, b) = (x:a, y:b)
gmmlcAcc Nothing (a, b) = ('1':a, '0':b)

getMaybeMostLeastCommon :: [String] -> (String, String)
getMaybeMostLeastCommon input =
  foldr gmmlcAcc ("", "")
  $ map (\x -> toMostLeastCommon (int2Float x) (int2Float (length input) / 2))
  $ foldl calc (replicate (length $ head input) 0) input

calc :: [Int] -> String -> [Int]
calc acc x = zipWith (+) acc (map digitToInt x)

result :: String -> String -> Int
result x y = toDecimal x * toDecimal y

-- invert :: [[a]] -> [[a]]
-- invert [] = []
-- invert [x] = map (: []) x
-- invert (x:xs) = map (\y -> map (: y) x) $ invert xs

matchAt :: String -> String -> Int -> Bool
matchAt a b i = (a !! i) == (b !! i)

filterOgrCsr :: [String] -> String -> Int -> [String]
filterOgrCsr input a i = filter (\x -> matchAt a x i) input

ogr :: [String] -> Int -> String
ogr [x] _ = x
ogr input i = do
  let mc = fst $ getMaybeMostLeastCommon input
  ogr (filterOgrCsr input mc i) (i + 1)

csr :: [String] -> Int -> String
csr [x] _ = x
csr input i = do
  let mc = snd $ getMaybeMostLeastCommon input
  ogr (filterOgrCsr input mc i) (i + 1)

part1 :: [String] -> Int
part1 = uncurry result . getMostLeastCommon

part2 :: [String] -> Int
part2 input = result (ogr input 0) (csr input 0)

main :: IO ()
main = do
  input <- lines <$> readFile "./test.txt"
  -- print (filter (uncurry (==) . head) [[('1', '1'), ('1', '0')], [('1', '1'), ('0', '0')]])
  

  -- 1st iter
  -- print (fst $ getMaybeMostLeastCommon input)
  let temp = filterOgrCsr input (fst $ getMaybeMostLeastCommon input) 0
  -- print temp

  print (foldl calc (replicate (length $ head temp) 0) temp)
  print (map (\x -> toMostLeastCommon (int2Float x) (int2Float (length temp) / 2))
    $ foldl calc (replicate (length $ head temp) 0) temp)
  
  -- 2nd iter
  print (fst $ getMaybeMostLeastCommon temp)
  let temp2 = filterOgrCsr temp (fst $ getMaybeMostLeastCommon temp) 1
  -- print temp2

  -- print (ogr input 0)
  -- print (invert ["123", "456"])
  printf "Part1: %d\n" (part1 input)
  printf "Part2: %d\n" (part2 input)
