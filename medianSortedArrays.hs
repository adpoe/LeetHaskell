module MedianSortedArray where
import Data.Array

getMedian :: Array Int Int -> Int
getMedian xs = 0


-- From comments at: http://www.jacobsheehy.com/2009/03/binary-search-in-haskell/
binsearch :: (Ord a) => [a] -> a -> Int -> Int -> Maybe Int
binsearch xs value low high
  | high value = binsearch xs value low (mid-1)
  | xs!!mid < value = binsearch xs value (mid+1) high
  | otherwise = Just mid
    where
        mid = low + ((-) high low `div` 2)
