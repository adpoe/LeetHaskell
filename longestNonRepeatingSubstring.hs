-- NonRepeatingSubstr Solution
-- Imports
module NonRepeatingSubstr where
import Data.List

{- WORKING SOLUTION in 12 lines of Haskell-}
substrList :: String -> [String]
substrList []       = []
substrList (x:xs)
  | cleanedSubstr == substr = (x:substr) : substrList xs
  | otherwise               = substrList xs
  where
    substr = Data.List.takeWhile (x /=) xs
    cleanedSubstr = Data.List.nub substr

longestSubstr :: String -> Int
longestSubstr s = length longest
  where
    strList = substrList s
    longest = foldl1' (\acc x -> if length acc > length x then acc else x) strList
    -- from: http://stackoverflow.com/questions/27528730/how-can-i-find-the-longest-string-within-a-list-haskell
