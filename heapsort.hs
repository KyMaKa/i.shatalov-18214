import Data.List
heapSort:: Ord a => [a] -> [a]
heapSort xs = heapHlp xs ((length xs) `div` 2 - 1) (length xs - 1) (length xs - 1)
  where
    heapHlp:: Ord a => [a] -> Int -> Int -> Int -> [a]
    heapHlp xs (-1) bot _ = sortheap bot xs
    heapHlp xs root bot maxChild | root * 2 <= bot = heap xs root bot maxChild
                                 | otherwise = heapHlp xs (root - 1) bot maxChild

    heap:: Ord a => [a] -> Int -> Int -> Int -> [a]
    heap xs root bot maxChild | root * 2 == bot = helper xs root bot (root * 2)
                              | xs !! (root * 2) > xs !! (root * 2 + 1) = helper xs root bot (root * 2)
                              | otherwise = helper xs root bot (root * 2 + 1)

    helper xs root bot maxChild | xs !! root < xs !! maxChild = heapHlp (swap root maxChild xs) maxChild bot maxChild
                                | otherwise = heapHlp xs (root - 1) bot maxChild

    swap i j xs | i == j = xs
    swap i j xs | otherwise = initial ++ (xs !! b) : middle ++ (xs !! a) : end
                where [a,b] = sort [i,j]
                      initial = take a xs
                      middle  = take (b-a-1) (drop (a+1) xs)
                      end = drop (b+1) xs

    sortheap:: Ord a => Int -> [a] -> [a]
    sortheap 0 xs = xs
    sortheap size xs = heapHlp (swap 0 size xs) 0 (size - 1) (size - 1)
