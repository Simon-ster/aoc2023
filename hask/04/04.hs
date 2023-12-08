import System.IO
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List as L
import Control.Applicative
import Data.Foldable as F
import Data.List.Split
import Debug.Trace
import Data.Set(member,fromList)
import Control.Applicative

main = do
    contents <- readFile "../../input/4.txt" 
    let matches =  apply part1 contents
    print $ sum matches
    print $ part2 (repeat 0) (lines contents)

apply f = map f . lines

part1 s = foldr (calcScore wins) 0 mine
    where (wins,mine) = parseInput s

part2 copyList [s] = 1 + (head copyList)

part2 copyList (s:ss) = copies + part2 newCopies ss
    where matches = numMatches s
          copies = 1 + head copyList
          restCopies = ZipList $ drop 1 copyList ++ repeat 0
          matchList = ZipList $ replicate matches copies ++ repeat 0
          newCopies = getZipList $ pure (+) <*> restCopies <*> matchList

numMatches s = foldr (\x -> if x `member` wins then (+1) else (+0)) 0 mine
    where (wins,mine) = parseInput s

parseInput ss = (wins,mine)
    where [w,m] = words <$> splitOn "|" ss
          wins = fromList $ drop 2 w
          mine = fromList $ m

calcScore wins myValue score | score == 0 = fromEnum $ member myValue wins
                             | member myValue wins = score * 2
                             | otherwise = score
onFirst f []     = []
onFirst f [s]    = [f s]
onFirst f (s:ss) = f s : ss
