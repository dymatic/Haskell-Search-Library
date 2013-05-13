module StatAnal.WordStat(
	  refPos
	, letterNumber
	, wordNumber
	, isSimilarWord 
	, isSimilarList
	, mostSimilarWord
	, similarLetters
	, score
	) where

import Data.List
import LibHaskell.Math.LibAlgebra
import LibHaskell.LibLists

--Returns first of a list in the event of a failure.

letterNumber :: Char -> Int 
letterNumber x = refPos (' ':['a'..'z'] ++ ['A'..'Z']) [0..52] x

wordNumber :: String -> Int
wordNumber xs = sum $ map letterNumber xs

-- Is length the same? Or is the word similar?
isSimilarWord :: String -> String -> Bool
isSimilarWord w1 w2 = (100 - (score (w1,w2) [(similarLetters,33),
										         (ses,33),
										         (similarLengths,33)])) <= 33

								
isSimilarList :: [String] -> [String] -> Bool
isSimilarList xs ys = let values = map (\(a,b) -> isSimilarWord a b) $ zip xs ys
					  in mostly values True

mostSimilarWord :: [String] -> String -> String
mostSimilarWord xs y = (fst (grab (byRight tups)))
	where tups = sew xs $ map (\c -> ((abs $ (wordNumber y - wordNumber c)) + ((abs ((length y) - (length c)))*3))) xs

similarLetters :: String -> String -> Bool
similarLetters x y = (abs (wordNumber x) - (wordNumber y)) <= (abs ((length x) - (length y)))  

similarLengths :: String -> String -> Bool
similarLengths a b = and [(d < la),
						  (d < lb)]
	where
		d = (abs $ (length a) - (length b))
		la = (length a)
		lb = (length b)

score :: ([a],[a]) -> [(([a] -> [a] -> Bool),Int)] -> Int
score (a,b) w = sum [c | (f,c) <- w, (f a b)]
