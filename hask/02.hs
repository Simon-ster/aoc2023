import System.IO
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import Control.Applicative
import Data.Foldable
import Data.List.Split
import Debug.Trace


getMaxColor "blue" = 14
getMaxColor s | s == "red" = 12
              | s == "green" = 13
              | otherwise = 0

main = do
    contents <- readFile "../input/2.txt"

    print $ solve part1 contents
    print $ solve part2 contents

solve f = sum . map f . zip [1..] . lines

validColor c x =
    last s /= c ||
    read (head s) <= getMaxColor c
  where s = words x

validColors s = and $
    validColor <$> rgb <*> splitOn "," s
  where rgb = words "red green blue"

part1 (n,s)
    | and $ validColors <$> splitOn ";" colors = n
    | otherwise = 0
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

power s = r * g * b
      where (r,g,b) = minColors $ words $ concat $ splitOn "," s

part2 (n,s) = power $ concat $ splitOn ";" colors
    where colors = last $ splitOn ":" s

