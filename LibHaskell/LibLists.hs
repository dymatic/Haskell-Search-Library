
module LibHaskell.LibLists(
  strt
 ,pos
 ,posList
 ,delete
 ,delFirst
 ,insert
 ,occurences
 ,inFst
 ,look
 ,one
 ,filterBreak
 ,after
 ,coltil
 ,churn
 ,sew
 ,append
 ,appendList
 ,spay
 ,dropAfter
 ,between
 ,removeBetween
 ,removeAllBetween
 ,bombard
 ,rm
 ,rmAll
 ,remBetwix
 ,remAllBetwix
 ,mostly
) where

-- For general lists not biased to a type.
strt :: (Eq a) => [a] -> Int -> [a]
strt [] _ = []
strt x 0 = x
strt (x:xs) t
	| t == 1 = xs
	| otherwise = strt xs (t - 1)

pos :: (Eq a) => [a] -> a -> Int
pos [] _ = -1
pos (x:xs) t
	| notElem t (x:xs) = -1
	| t == x = 0
	| otherwise = 1 + pos xs t

posList :: (Eq a) => [a] -> [a] -> Int
posList [] _ = -1
posList a@(x:xs) y
	| (take (length y) a) == y = 0
	| otherwise = 1 + posList xs y

delete :: (Eq a) => [a] -> [a] -> [a]
delete [] _ = []
delete a@(x:xs) y
	| y == take (length y) a = delete (strt a (length y)) y
	| otherwise = x : delete xs y

delList :: (Eq a) => [a] -> [[a]] -> [a]
delList a [] = a
delList xs (y:ys) = delList ( delete xs y) ys

delFirst :: (Eq a) => [a] -> [a] -> [a]
delFirst a@(x:xs) ys
	| take (length ys) a == ys = strt xs $ (length ys) - 1
	| otherwise = x : delFirst xs ys

-- Indices start at 0
insert :: (Eq a) => [a] -> Int -> [a] -> [a]
insert xs wh ys = take (wh) xs ++ ys ++ strt xs wh

occurences :: (Eq a) => [a] -> a -> Int
occurences xs y = sum $ map (\a -> if a == y then 1 else 0) xs

inFst :: (Eq a) => a -> [(a,b)] -> Bool
inFst a xs = a `elem` (map fst xs)

--Associative Tupple Lookup
look :: (Eq a) => a -> [(a,a)] -> a
look t xs = snd $ one (filter (\(a,b) -> a == t) xs) 

one :: [a] -> a
one (x:xs) = x

filterBreak :: (a -> Bool) -> [a] -> [a]
filterBreak _ [] = []
filterBreak fun (x:xs)
  | not (fun x) = []
  | otherwise = x : filterBreak fun xs

after :: (Eq a) => [a] -> a -> [a]
after x y = strt x $ 1 + (pos x y)

coltil :: (Eq a) => [a] -> a -> [a] -- Collect until you meet an element
coltil x y = take (pos x y) x

churn :: a -> Int -> [a]
churn x n
  | (n == 1) = x:[]
  | otherwise = x:churn x (n - 1)
  
  
sew :: [a] -> [b] -> [(a,b)]
sew [] _ = []
sew _ [] = []
sew (x:xs) (y:ys) = (x,y) : sew xs ys

append :: [a] -> a -> [a]
append [] b = b:[]
append (x:xs) b = x : append xs b

appendList :: [a] -> [a] -> [a]
appendList [] b = b++[]
appendList (x:xs) b = x : appendList xs b

spay :: [a] -> [a]
spay [] = []
spay a@(x:xs)
  | (length a) == 2 = x:[]
  | otherwise = x : spay xs
  
dropAfter :: (Eq a) => [a] -> a -> [a]
dropAfter [] _ = []
dropAfter (x:xs) b
  | (x == b) = x:[]
  | otherwise = x : dropAfter xs b

between :: (Eq a) =>  [a] -> (a,a) -> [a]
between [] _ = []
between xs (a,b) = spay $ dropAfter (after xs a) b

removeBetween ::(Eq a) =>  [a] -> (a,a) -> [a]
removeBetween xs (a,b) = delete xs $ between xs (a,b)

removeAllBetween :: (Eq a) => [a] -> (a,a) -> [a]
removeAllBetween xs (a,b)
  | and [(a `elem` xs),(b `elem` xs)] = removeAllBetween (removeBetween xs (a,b)) (a,b)
  | otherwise = xs
  
bombard :: [([a] -> [a])] -> [a] -> [a]
bombard [] ys = ys
bombard (x:xs) ys = x (bombard xs ys)

-- Delete for single elements
rm :: (Eq a) => [a] -> a -> [a]
rm [] _ = []
rm (x:xs) b
  | (x == b) = rm xs b
  | otherwise = x : rm xs b

rmAll :: (Eq a) => [a] -> [a] -> [a]
rmAll a [] = a
rmAll xs (b:bs) = rmAll (rm xs b) bs

--The following function is like removeBetween, but it also removes (a,b)
remBetwix :: (Eq a) => [a] -> (a,a) -> [a]
remBetwix xs (a,b) =  rmAll (removeBetween xs (a,b)) [a,b] 

--Deletes every occurance
remAllBetwix :: (Eq a) => [a] -> (a,a) -> [a]
remAllBetwix xs (a,b)
  | and [(a `elem` xs),(b `elem` xs)] = remAllBetwix (remBetwix xs (a,b)) (a,b)
  | otherwise = xs

mostly :: (Eq a) => [a] -> a -> Bool
mostly xs n = ncur > (length xs) - ncur
  where ncur = occurences xs n





















