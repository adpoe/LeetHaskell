-- TwoSum Solution
module TwoSum where
import Data.List

-- defns
testCase :: [Int]
testCase = [3,2,4]

type Target = Int
type Index = Int

natNums :: [Int]
natNums = [1..]

-- fns
twoSum :: [Int] -> Target -> (Int,Int)
twoSum xs target = indices
  where
    idxs = getIndices xs natNums
    pairs = getAllPairs idxs
    indices = checkSum pairs target

getIndices :: [Int] -> [Int] -> [(Index, Int)]
getIndices [] _ = []
getIndices _ [] = []
getIndices (x:xs) (n:ns) = (n,x) : getIndices xs ns

getAllPairs :: [(Index, Int)] -> [((Index, Int), (Index, Int))]
getAllPairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

checkSum :: [((Index, Int), (Index, Int))] -> Int -> (Index, Index)
checkSum [] _ = (-1, -1)
checkSum (x:xs) target
  | (+) firstNum secondNum == target = (idxOne, idxTwo)
  | otherwise = checkSum xs target
    where
      firstNum = snd $ fst x
      secondNum = snd $ snd x
      idxOne = fst $ fst x
      idxTwo = fst $ snd x
