import System.IO
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import Control.Applicative
import Data.Foldable
import Data.List.Split
import Debug.Trace

main = do
    contents <- readFile "../../input/3.txt" 

    let collections = getCollections $ lines contents
    let maxY = length $ collections
    let maxX = length $ head collections
    let allPoints = concat collections

    let symbols = filterSymbols allPoints
    print $ sum $ getValidNumbers maxX maxY symbols allPoints

surrounds w h (x,y) = filter validPos [l,r,u,d,ul,ur,dl,dr]
        where validPos (a,b) = a < w && a >= 0 && b < h && b >= 0
              l = (x-1,y)
              r = (x+1,y)
              u = (x,y-1)
              d = (x,y+1)
              ul = (x-1,y-1)
              ur = (x+1,y-1)
              dl = (x-1,y+1)
              dr = (x+1,y+1)

getNums [(x,y,s)]  = [s]
getNums ((x,y,s):ns) = s : getNums(ns)

getPos (x,y,s) = (x,y)

nearbySymbol symbols x y positions = not $ null $ filter (`elem` (getPos <$> symbols)) $ concat $  surrounds x y <$> (positions))


getValidNumbers xmax ymax symbols [n] | isNum n && (nearbySymbol symbols xmax ymax [getPos n]) = [read $ show $ thrd n]
                                      | otherwise = []
getValidNumbers xmax ymax symbols (n:ns) | isNum n && (nearbySymbol symbols xmax ymax positions) = num : (getValidNumbers xmax ymax symbols rest)
                                         | isNum n = getValidNumbers xmax ymax symbols rest
                                        
                                         | otherwise = getValidNumbers xmax ymax symbols ns 
                                        where 
                                        collection = takeWhile (isNumAndInLine (frst n)) (n:ns) where
                                        rest = dropWhile (isNumAndInLine (frst n)) (n:ns)
                                        positions = getPos <$> collection
                                        num = read $ getNums collection :: Integer
getValidNumbers _ _ _ _ = []



getCollections ss = createCollection <$> (zip [0..] ss)

createCollection (n, s) = (\(r,(c,sym)) -> (r,c,sym)) <$> (zip (repeat n) (zip [0..] s))


filterSymbols = filter (isValidSymbol . thrd) 

isNum = isDigit . thrd

frst (a,_,_) = a
thrd (_,_,c) = c

isNumAndInLine c (x,y,z) = x==c && isDigit z

isValidSymbol c = not $ isAlphaNum c || c == '.'
