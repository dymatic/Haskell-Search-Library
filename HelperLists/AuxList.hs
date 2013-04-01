module HelperLists.AuxList(
	  pos
	 ,subList
	 ,splitInto
	 ,after
	 ,filterBreak
	 ,split
	) where

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

split :: String -> Char -> [String]
split xs c = filter (/="") (splitOn xs c)

splitOn :: String -> Char -> [String]
splitOn [] _ = [""]
splitOn xs c
	| c `elem` xs = (filterBreak (/= c) xs) : splitOn (after xs c) c
	| otherwise = xs:[""]