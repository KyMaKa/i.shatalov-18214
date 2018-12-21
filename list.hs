myGet:: (Ord t, Num t) => [p] -> t -> p
myGet [] _ = error "Ur list is empty"
myGet (x:xs) n = if n > 1 then myGet (xs) (n - 1)
  else (x)

myHead :: [a] -> a
myHead [] = error "Ur list is empty"
myHead (x:xs) = x

myLast:: [a] -> a
myLast [] = error "Ur list is empty"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

myTail:: [a] -> [a]
myTail [] = error "Ur list is empty"
myTail (x:xs) = xs

myInit:: Num a => [a] -> [a]
myInit [] = error "Ur list is empty"
myInit (x:[]) = []
myInit (x:xs) = x:(myInit xs)

myReverse:: Ord a => [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse' (x:xs) []
  where
    myReverse' :: Ord a => [a] -> [a] -> [a]
    myReverse' (x:xs) ys = myReverse' xs (x:ys)
    myReverse' [] ys = ys

myLength:: Ord a => [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myAppend:: Ord a => [a] -> a -> [a]
myAppend [] x = [x]
myAppend (y:xs) x = myAppend' (y:xs) []
  where
    myAppend' (y:xs) ys = myAppend' xs (y:ys)
    myAppend' [] (a:ys) = myAppend'' (a:ys) [x]
      where
        myAppend'' (a:ys) zs = myAppend'' ys (a:zs)
        myAppend'' [] zs = zs

myConcat:: Ord a => [a] -> [a] -> [a]
myConcat [] [] = []
myConcat [] (x:xs) = myConcat (x:xs) []
myConcat (x:xs) sys = myConcat' (x:xs) []
  where
    myConcat' (x:xs) ys = myConcat' xs (x:ys)
    myConcat' [] (y:ys) = myConcat'' (y:ys) sys
      where
        myConcat'' (y:ys) sys = myConcat'' ys (y:sys)
        myConcat'' [] sys = sys

myDrop:: Ord a => Int -> [a] -> [a]
myDrop _ [] = error "Ur list is empty"
myDrop 0 (xs) = xs
myDrop n (x:xs) = myDrop (n-1) xs

myTake:: Int -> [a] -> [a]
myTake _ [] = error "Ur list is empty"
myTake n (x:xs) = if n /= 0 then x : myTake (n-1) xs
  else
    []

mySplitAt:: Ord a => Int -> [a] -> ([a], [a])
mySplitAt n (x:xs) = (myTake n (x:xs), myDrop n (x:xs))

myNull:: Ord a => [a] -> Bool
myNull xs = if xs == [] then True
  else False

myElem:: Ord a => [a] -> a -> Bool
myElem [] _ = False
myElem (y:xs) x = myElem' (y:xs)
  where
    myElem' [] = False
    myElem' (y:xs) = if x == y then True
      else
        myElem' xs

myFilter:: Eq a => (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter test (x:xs) = if (test x) then (x : myFilter' [])
  else myFilter test xs
    where
      myFilter' ys = if xs /= [] then myFilter test xs
  			else ys

myMap:: Eq a => (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = ((f x):myMap' [])
	where
		myMap' ys = if xs /= [] then myMap f xs
			else ys


transpose':: Eq a => [[a]] -> [[a]]
transpose' [] = []
transpose' ([] : xs) = transpose' xs
transpose' ys = (myMap head' ys) : transpose' (myMap tail' ys)
  where
    head':: [a] -> a
    head' (x:_) = x
    tail' (_:xs) = xs


intersperse:: a -> [a] -> [a]
intersperse _   [] = []
intersperse sep (x:xs)  = x : help sep xs
  where
    help _ [] = []
    help sep (x:xs) = sep : x : help sep xs
