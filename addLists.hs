-- adLists Solution
module AddLists where

n1 :: [Integer]
n1 = [2, 4, 3]

n2 :: [Integer]
n2 = [5,6,4]

addNums :: [Integer] -> [Integer] -> [Integer]
addNums _ [] = []
addNums [] _ = []
addNums xs ys = carries
  where
    additions = firstPass xs ys
    carries = carry additions

firstPass :: [Integer] -> [Integer] -> [Integer]
firstPass _ [] = []
firstPass [] _ = []
firstPass (x:xs) (y:ys) = (+) x y : firstPass xs ys

carry :: [Integer] -> [Integer]
carry [] = []
carry (x:xs)
  | (>) x 9   = remainder : carryValue : carry (tail xs)
  | otherwise = x : carry xs
    where
      carryValue = (+) (head xs) $ mod x 9
      remainder = (-) x 10
