-- NonRepeatingSubstr Solution
-- Imports
module NonRepeatingSubstr where
import Data.List
import Data.Function (on)

-- from: http://stackoverflow.com/questions/19525022/all-subsequences-from-a-single-string
nonEmptySubstrings :: String -> [String]
nonEmptySubstrings = concatMap (tail . inits) . tails

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

getPalindromes :: String -> [String]
getPalindromes s = filter isPalindrome $ nonEmptySubstrings s

longestPalindrome :: String -> String
longestPalindrome s = head . reverse . sortBy (compare `on` length) $ getPalindromes s
