myGet:: (Ord t, Num t) => [p] -> t -> p
myGet (x:xs) n = if n > 1 then myGet (xs) (n - 1)
  else (x)

myHead :: [a] -> a
myHead (x:xs) = x

myLast:: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs

myTail:: [a] -> [a]
myTail (x:xs) = xs

myInit:: Num a => [a] -> [a]
myInit (x:[]) = []
myInit (x:xs) = x:(myInit xs)

myReverse:: Ord a => [a] -> [a]
myReverse (x:xs) = myReverse' (x:xs) []
  where
    myReverse' :: Ord a => [a] -> [a] -> [a]
    myReverse' (x:xs) ys = myReverse' xs (x:ys)
    myReverse' [] ys = ys

myLength:: Ord a => [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myAppend:: Ord a => [a] -> a -> [a]
myAppend (y:xs) x = myAppend' (y:xs) []
  where
    myAppend' (y:xs) ys = myAppend' xs (y:ys)
    myAppend' [] (a:ys) = myAppend'' (a:ys) [x]
      where
        myAppend'' (a:ys) zs = myAppend'' ys (a:zs)
        myAppend'' [] zs = zs

myConcat:: Ord a => [a] -> [a] -> [a]
myConcat (x:xs) sys = myConcat' (x:xs) []
  where
    myConcat' (x:xs) ys = myConcat' xs (x:ys)
    myConcat' [] (y:ys) = myConcat'' (y:ys) sys
      where
        myConcat'' (y:ys) sys = myConcat'' ys (y:sys)
        myConcat'' [] sys = sys

myDrop:: Ord a => Int -> [a] -> [a]
myDrop 0 (xs) = xs
myDrop n (x:xs) = myDrop (n-1) xs

myTake:: Int -> [a] -> [a]
myTake n (x:xs) = if n /= 0 then x : myTake (n-1) xs
  else
    []
