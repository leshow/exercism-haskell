module HowManyNumbersIII where

import           Data.Foldable
import           Control.Monad


--       We want to generate all the numbers of three digits where:
--       the sum of their digits is equal to 10.
--       their digits are in increasing order (the numbers may have two or more equal contiguous digits)
--   The numbers that fulfill the two above constraints are: 118, 127, 136, 145, 226, 235, 244, 334
--   Make a function that receives two arguments:
--       the sum of digits value
--       the desired number of digits for the numbers
--   The function should output an array with three values: [1,2,3]
--   1 - the total number of possible numbers
--   2 - the minimum number
--   3 - the maximum number
--   The example given above should be:
--   findAll 10 3 -> ( 8, Just 118, Just 334 )
-- 
-- Backtracking in haskell:
-- https://www.codewars.com/kata/how-many-numbers-iii/
-- combinations problem
go :: Int -> Int -> Int -> [[Int]]
go total n start
    | total < start = []
    | n == 1 = pure [total]
    | otherwise = do
        x <- [start .. 9]
        (x :) <$> go (total - x) (n - 1) x

fromDigitList :: Num a => [a] -> a
fromDigitList = foldl' (\num d -> 10 * num + d) 0

findAll :: Int -> Int -> (Int, Maybe Int, Maybe Int)
findAll sum n | len == 0  = (0, Nothing, Nothing)
              | otherwise = (len, Just $ minimum xs, Just $ maximum xs)
  where
    len = length xs
    xs  = (fromDigitList <$> (go sum n 1))
