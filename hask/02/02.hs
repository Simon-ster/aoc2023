import System.IO
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import Control.Applicative
import Data.Foldable
import Data.List.Split
import Debug.Trace


getMaxColor s | s == "red" = 12
              | s == "blue" = 14
              | s == "green" = 13
              | otherwise = 0

main = do
    contents <- readFile "../../input/2.txt"

    print $ solve part1 contents
    print $ solve part2 contents

solve f = sum . map (line f) . zip [1..] . lines

line :: (Eq a, Eq b, Num a) => ((a,b) -> Maybe a) -> (a,b) -> a
line f s | res == Nothing = 0
         | otherwise = fromJust res
    where res = f s

validColor c x | (last s == c) = ((read $ head s) <= getMaxColor c)
           | otherwise = True
           where s = words $ x

validColors s =
    (and $ validColor "red" <$> splitOn "," s) &&
    (and $ validColor "green" <$> splitOn "," s) && 
    (and $ validColor "blue"<$> splitOn "," s)

part1 (n,s) 
    | and $ validColors <$> splitOn ";" colors = Just n
    | otherwise = Nothing
    where colors = last $ splitOn ":" s

getMax i []       = 0
getMax i [n,c]    | i == c    = read n
                  | otherwise = 0
getMax i (n:c:cs) | i == c = max (read n) (getMax i cs)
                  | otherwise = getMax i cs

minColors s = (red, blue, green)
    where red   = getMax "red"   s
          blue  = getMax "blue"  s
          green = getMax "green" s 

power s = Just (r * g * b)
      where (r,g,b) = minColors $ words $ concat $ splitOn "," s

part2 (n,s) = power $ concat $ splitOn ";" colors
    where colors = last $ splitOn ":" s

