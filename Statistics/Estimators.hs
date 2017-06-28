-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Data.List
--import Data.Map qualified as M
split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split cs delim
r x = read x :: Int
conv x = map r (split x ' ')
mean l = fromIntegral(sum (conv l)) / (fromIntegral (length (conv l)))
avgmid :: [Int] -> Float
avgmid x = ((medianElem x) + (medianElem2 x)) / 2.0
-- (fromIntegral(x!!( div (length x) 2 ) + x!!((div (length x) 2) - 1)))/2
mid :: [Int] -> Float
mid x = medianElem x
-- fromIntegral(x!!(div (length x) 2) )
medianUB x = div (length x) 2 
medianLB x = (medianUB x) - 1
medianElem x = fromIntegral(x!!(medianLB x))
medianElem2 x = fromIntegral(x!!(medianUB x))
median l  
 | mod (length sl) 2 == 0 = (avgmid sl)
 | otherwise = mid sl
 where sl = sort $ conv l
median' l  
 | mod (length sl) 2 == 0 = (avgmid' sl)
 where sl = conv l
avgmid' x = x!!( div (length x) 2 )  
mode l = mode' $ conv l
mode' y = (snd $ maximum $ map (\x -> (length x, -1 * x!!0 )) $ sort $ group y) * (-1)
f x = sort $ map (\x -> (length x, x!!0) )  x
g y =  -1 * (snd $ last $ sort $ map (\x -> (length x, -1 * x!!0)) (group $ sort y))
main = do  
    count <- getLine
    numbers <- getLine  
    print $ mean numbers
    print $ median numbers
    --print $ mode numbers
    --print "Debug"
    --print $ f $ group $ sort $ conv numbers 
    --print "Debug2"
    print $ g $ conv numbers 