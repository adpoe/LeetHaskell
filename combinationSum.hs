module CombinationSum where
import Data.List
-- From: https://leetcode.com/problems/combination-sum-iv/

-- ns = [1,2,3]
-- t = 4
-- perms  =  Data.List.permutations $ getFullList ns t

combinationSum :: [Int] -> Int -> Int
combinationSum ns t = solns
  where
    perms  =  Data.List.permutations $ getFullList ns t
    valids = sumsLessThan perms t
    solns = length valids

sumsLessThan :: [[Int]] -> Int -> [[Int]]
sumsLessThan perms targ = nub $ concat [getValidsSubSqns p targ | p <- perms]

getValidsSubSqns :: [Int] -> Int -> [[Int]]
getValidsSubSqns p t = validLists
  where
    sqns = subsequences p
    sums = map sum sqns
    results = zip sums sqns
    valids = filter (\h -> t == fst h) results
    validLists = map snd valids

getFullList :: [Int] -> Int -> [Int]
getFullList xs targ = concat [findMaxDupes x targ | x <- xs]

findMaxDupes :: Int -> Int -> [Int]
findMaxDupes n t = last $ maxNums n t

maxNums :: Int -> Int -> [[Int]]
maxNums n t = takeWhile (\l -> sum l <= t) $ listGrow n []

listGrow :: Int -> [Int] -> [[Int]]
listGrow n [] = [n] : listGrow n [n]
listGrow n xs = oneMore : listGrow n oneMore
  where
    len = length xs
    newSize = (+) len 1
    oneMore = replicate newSize n
