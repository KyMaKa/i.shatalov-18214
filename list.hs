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


-- mySplitAt n (x:xs) = mySplitAt' n (x:xs)
 -- where
   -- mySplitAt' n (x:xs) = if n > 1 then mySplitAt'' (x:[]) (x:xs)
     -- else ys xs
      -- where
        -- mySplitAt'' ys (xs) = mySplitAt' (n - 1) xs

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
        
-- myFilter test xs = 

myMap:: Eq a => (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = ((f x):myMap' [])
	where 
		myMap' ys = if xs /= [] then myMap f xs
			else ys
