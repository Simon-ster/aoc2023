import System.IO
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List as L
import Control.Applicative
import Data.Foldable as F
import Data.List.Split
import Debug.Trace
import Data.Set as S

main = do
    contents <- readFile "../../input/3.txt" 

    let points = createPoints $ lines contents

    let symbols = filterSymbols points
    let numPos = getNumberPositions points

    print $ (sum) $ fst <$> L.filter (nearbySymbol symbols) numPos

    let gears = L.filter (\s -> (thrd s == '*')) symbols
    let numberCandidates = L.filter (nearbySymbol gears) numPos
    print $ calcGears numberCandidates gears

calcGears nums [g]    | isPair = (head cands) * (head $ tail cands)
                      | otherwise = 0
            where nextTo = S.fromList $ surrounds (frst g,scnd g)
                  cands = findPair nums nextTo
                  isPair = length cands == 2

calcGears nums (g:gs) | isPair = (head cands) * (head $ tail cands) + (calcGears nums gs)
                      | otherwise = calcGears nums gs
            where nextTo = S.fromList $ surrounds (frst g,scnd g)
                  cands = findPair nums nextTo
                  isPair = length cands == 2

findPair [(v,ps)] set    = []
findPair ((v,ps):ns) set | posIn set ps = v : findSecond ns set
                         | otherwise = findPair ns set

findSecond [(v,ps)] set | posIn set ps = [v]
                        | otherwise    = []
findSecond ((v,ps):ns) set | posIn set ps = v : findSecond ns set
                           | otherwise    = findSecond ns set


posIn set ps = any (`S.member` set) ps


surrounds (x,y) = [l,r,u,d,ul,ur,dl,dr]
            where
              l = (x-1,y)
              r = (x+1,y)
              u = (x,y-1)
              d = (x,y+1)
              ul = (x-1,y-1)
              ur = (x+1,y-1)
              dl = (x-1,y+1)
              dr = (x+1,y+1)


nearbySymbol symbols input = not $ F.null $ L.filter inSymbols $ concat $ surrounds <$> positions 
            where 
            positions = snd input
            inSymbols  = (`elem` (getPos <$> symbols))
                                

getNumberPositions []  = []
getNumberPositions [(x,y,n)] | isDigit n = [(ord n, [(x,y)])]
                             | otherwise = []

getNumberPositions (n:ns) | isNum n = numPos : (getNumberPositions next)
                          | otherwise = getNumberPositions ns
                        where next = dropWhile (isNumAndInLine n) (n:ns)
                              sepNumPos = takeWhile (isNumAndInLine n) (n:ns)
                              poss = getPos <$> sepNumPos
                              num = read $ getNums sepNumPos
                              numPos = (num,poss)

createPoints lines = do
    (y, row) <- zip [0..] lines
    (x, value) <- zip [0..] row
    return (x,y,value)

filterSymbols = L.filter (isValidSymbol . thrd) 

isNum = isDigit . thrd

frst (a,_,_) = a
scnd (_,b,_) = b
thrd (_,_,c) = c

isNumAndInLine n (x,y,z) = y==(scnd n) && isDigit z

isValidSymbol c = not $ isAlphaNum c || c == '.'

getNums []         = []
getNums [(x,y,s)]  = [s]
getNums ((x,y,s):ns) = s : getNums(ns)

getPos (x,y,s) = (x,y)
