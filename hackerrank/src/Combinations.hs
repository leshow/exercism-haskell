module Combinations where

-- https://leetcode.com/problems/combinations/

combine :: Int -> [Int] -> [[Int]]
combine 0 _        = [[]]
combine _ []       = []
combine k (x : xs) = ((x :) <$> combine (k - 1) xs) ++ combine k xs



