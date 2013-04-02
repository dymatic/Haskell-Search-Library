module HelperLists.AuxList(
	  numberOf
	 ,numberNot
	 ,mostly
	 ,allEq
	 ,pos
	 ,subList
	 ,splitInto
	 ,after
	 ,filterBreak
	 ,splitOn
	) where

--numberOf :: (Eq a) => [a] -> a -> Int
--numberOf [] _ = 0
--numberOf (x:xs) y
--	| x == y = 1 + numberOf xs y
--	| otherwise = numberOf xs y

numberOf :: (Eq a) => [a] -> a -> Int
numberOf xs t = sum $ map (\n -> if n == t then 1 else 0) xs

numberNot :: (Eq a) => [a] -> a -> Int
numberNot xs t = sum $ map (\n -> if n == t then 0 else 1) xs

mostly :: (Eq a) => [a] -> a -> Bool
mostly (xs) y = numberOf xs y > numberNot xs y

allEq :: (Eq a) => [a] -> a -> Bool
allEq [] _ = True
allEq (x:xs) y
	| x /= y = False
	| otherwise = allEq xs y

pos :: (Eq a) => [a] -> a -> Int
pos [] _ = -1
pos (x:xs) y
    | not $ elem y (x:xs) = -1
    | x == y =  0
    | otherwise = 1 + pos xs y

subList :: [a] -> Int -> [a]
subList a 0 = a
subList (_:xs) for = subList xs (for - 1)

splitInto :: String -> Int -> [String]
splitInto a c
	| length a < c = []
	| otherwise = (take c a):splitInto (subList a c) c

after :: (Eq a) => [a] -> a -> [a]
after lst nod = subList lst (1 + pos lst nod)

filterBreak :: (a -> Bool) -> [a] -> [a]
filterBreak _ [] = []
filterBreak f (x:xs) 
	| (f x) = x : filterBreak f xs
	| otherwise = []

splitOn :: String -> Char -> [String]
splitOn [] _ = []
splitOn xs c
	| c `elem` xs = (filterBreak (/= c) xs) : splitOn (after xs c) c
	| otherwise = xs:[]