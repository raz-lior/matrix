module Matrix(Matrix
            , showMatrix
            , initMatrix
            , gaussianElimination) where

import Data.List
import Data.Maybe
import System.Random
import Debug.Trace

type Matrix = [[Double]]

showMatrix :: Matrix -> String
showMatrix [] = ""
showMatrix m = foldl (\acc x -> acc ++ "\n" ++ show x) (show (head m)) (tail m)

initMatrix :: Int -> Int -> Matrix
initMatrix _ 0 = []
initMatrix 0 _ = []
initMatrix col row = randomList col row : initMatrix col (row-1)

gaussianElimination :: Matrix -> Matrix
gaussianElimination [] = []
gaussianElimination [a] = forwardReduction [a] []
gaussianElimination m = 
    (gaussianElimination'' . gaussianElimination') m

gaussianElimination' :: Matrix -> Matrix
gaussianElimination' [] = []
gaussianElimination' [a] = forwardReduction [a] []
gaussianElimination' m = 
     addColumn headColumn ( headRow : geMinor )
    where rowReduced = forwardReduction m []
          headColumn = (head . transpose) rowReduced
          headRow = if (null . tail . transpose) rowReduced then [] else (head . transpose . tail . transpose) rowReduced
          minor = if (null . tail . transpose) rowReduced then [] else (tail . transpose . tail . transpose) rowReduced
          geMinor = gaussianElimination' minor
          bottomRow = last geMinor


gaussianElimination'' :: Matrix -> Matrix
gaussianElimination'' [] = []
gaussianElimination'' [a] = reverseReduction [a] [] Nothing
gaussianElimination'' m = 
    geMatrix ++ [lastRow]
    where rowReduced = reverseReduction m [] Nothing
          lastRow = last rowReduced
          topMatrix = init rowReduced
          geMatrix = gaussianElimination'' topMatrix



addColumn :: [Double] -> Matrix -> Matrix
addColumn colmn m = transpose ( colmn : transpose m )

initMatrixRec :: Int -> Int -> Matrix -> Matrix
initMatrixRec _ 0 acc = acc
initMatrixRec col row acc = initMatrixRec col (row-1) (randomList col row : acc)

randomList :: Int -> Int -> [Double]
randomList n seed = take n (map fromIntegral (randomRs (0,10) (mkStdGen seed) :: [Int]))

forwardReduction :: Matrix -> [Double] -> Matrix
forwardReduction [] _ = []
forwardReduction (x:xs) [] = row : forwardReduction xs row
    where row = normalize x
forwardReduction (x:xs) h = zipWith ( \a b -> a - b * multiplier ) x h : forwardReduction xs h
    where multiplier = if head h == 0 then 0
                       else head x / head h

reverseReduction :: Matrix -> [Double] -> Maybe Int -> Matrix
reverseReduction [] _ _ = []
reverseReduction m [] _ = reverseReduction (init m) lastRow oElmIdx ++ [lastRow]
    where lastRow = last m
          oElmIdx = openingElemIdx lastRow
reverseReduction (x:xs) lr oElmIdx = zipWith ( \a b -> a - b * multiplier ) x lr : reverseReduction xs lr oElmIdx
    where multiplier = maybe 0 (\idx -> (x !! idx) / (lr !! idx)) oElmIdx

openingElemIdx :: [Double] -> Maybe Int
openingElemIdx [] = Nothing
openingElemIdx x = if len < length x then Just len
                   else Nothing
    where len = length $ takeWhile (== 0) x

normalize :: [Double] -> [Double]
normalize [] = []
normalize row = map (* mul) row
    where mul = maybe 1 (\idx -> 1 / row !! idx ) (openingElemIdx row)

reorder :: Matrix -> Matrix
reorder m 
    | firstElem m == 1 = m 
    | isNothing swapRowIdx = m
    | otherwise = moveToFront (fromJust swapRowIdx) m
    where swapRowIdx = findIndex (\list -> head list == 1) m

firstElem :: Matrix -> Double
firstElem = head . head

moveToFront :: Int -> [[a]] -> [[a]]
moveToFront idx m = (m !! idx) : take idx m ++ drop (succ idx) m

isCanonicalForm :: Matrix -> Bool
isCanonicalForm m = 1 == foldl (\acc x -> if x /= 0 then acc + 1 else acc ) 0 (head (transpose m))


