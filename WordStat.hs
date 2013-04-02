module WordStat(
	  refPos
	, letterNumber
	, wordNumber
	, isSimilarWord 
	, isSimilarList
	) where

import Data.List
import HelperMath.Aux
import HelperLists.AuxList


-- Get the same element from another range as one element of the first range.
refPos :: (Eq a) => [a] -> [b] -> a -> b
refPos r1 r2 e1 = r2 !! let x = (r1 `pos` e1) in (if x < 0 then 0 else x)
--Returns first of a list in the event of a failure.

letterNumber :: Char -> Int 
letterNumber x = refPos (' ':['a'..'z'] ++ ['A'..'Z']) [0..52] x

wordNumber :: String -> Int
wordNumber xs = sum $ map letterNumber xs

-- Is length the same? Or is the word similar?
isSimilarWord :: String -> String -> Bool
isSimilarWord w1 w2 = or [and 
							[stdDev [realToFrac ws1, realToFrac ws2] < realToFrac (lw1 + lw2),
						    abs (lw1 - lw2) < 5],
						  (lw1 == lw2)]
						where ws1 = wordNumber w1
						      ws2 = wordNumber w2
						      lw1 = length w1
						      lw2 = length w2

isSimilarList :: [String] -> [String] -> Bool
isSimilarList xs ys = let values = map (\(a,b) -> isSimilarWord a b) $ zip xs ys
					  in mostly values True