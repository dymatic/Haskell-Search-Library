module WordStat(
	 pos
	, refPos
	, letterNumber
	, wordNumber
	, isSimilarWord 
	) where
import Data.List
import HelperMath.Aux

pos :: (Eq a) => [a] -> a -> Int
pos [] _ = -1
pos (x:xs) y
    | not $ elem y (x:xs) = -1
    | x == y =  0
    | otherwise = 1 + pos xs y

-- Get the same element from another range as one element of the first range.
refPos :: (Eq a) => [a] -> [b] -> a -> b
refPos r1 r2 e1 = r2 !! let x = (r1 `pos` e1) in (if x < 0 then 0 else x)
--Returns first of a list in the event of a failure.

letterNumber :: Char -> Int 
letterNumber x = refPos (' ':['a'..'z'] ++ ['A'..'Z']) [0..52] x

wordNumber :: String -> Int
wordNumber xs = sum $ map letterNumber xs




isSimilarWord :: String -> String -> Bool
isSimilarWord w1 w2 = stdDev [realToFrac ws1, realToFrac ws2] < realToFrac ((length w1) + (length w2))
						where ws1 = wordNumber w1
						      ws2 = wordNumber w2