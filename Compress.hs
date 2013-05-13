module Compress(
	massRepeats
	,unRepeats
	) where

import LibHaskell.LibLists


massRepeats :: String -> String
massRepeats [] = []
massRepeats (x:"") = x:""
massRepeats a@(x:y:xs)
	| (length a) <= 1 = ""
	| x == y = ('(':x:(show sames)++")") ++ massRepeats (strt a sames)
 	| otherwise = x:(massRepeats (y:xs))
	where sames = (length (filterBreak (== x) a))

unRepeats :: String -> String
unRepeats [] = []
unRepeats (x:"") = x:""
unRepeats (x:y:ys)
	| (x == '(') = churn y ((read ((coltil (ys) ')')) ::Int)) ++ unRepeats (after ys ')')
	| otherwise = x:""++unRepeats (y:ys)