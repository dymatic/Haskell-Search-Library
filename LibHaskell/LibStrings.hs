module LibHaskell.LibStrings(
  splitOn
) where
import LibHaskell.LibLists
 --For lists of characters in particular.
splitOn :: String -> Char -> [String]
splitOn [] _ = []
splitOn xs c
	| c `elem` xs = (filterBreak (/= c) xs) : splitOn (after xs c) c
	| otherwise = xs:[]