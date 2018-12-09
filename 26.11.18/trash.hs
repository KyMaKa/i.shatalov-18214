import HeapSort
import Data.List
main = do
  line <- getLine
  if line /= "" then printList (heapSort (words (line))) else main
    where
      printList::[String]-> IO ()
      printList (x:xs) | xs == [] = putStrLn (show x)
                       | otherwise = (putStrLn (show x)) >> printList xs
