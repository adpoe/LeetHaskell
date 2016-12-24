module MedianSortedArray where
import Data.Array;
-- From comments at: http://www.jacobsheehy.com/2009/03/binary-search-in-haskell/

binarySearch :: Array Int Int -> Int -> Int -> Int -> Int -- list, value, low, high, return int
binarySearch haystack needle lo hi
  | hi < lo	= Nothing
  | pivot > needle	= binarySearch haystack needle lo (mid-1)
  | pivot < needle	= binarySearch haystack needle (mid+1) hi
  | otherwise	= Just mid
    where
      mid	= lo + (hi-lo) `div` 2
      pivot	= haystack!mid
