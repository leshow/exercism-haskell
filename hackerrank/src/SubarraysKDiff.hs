-- 992. Subarrays with K Different Integers
-- Hard

-- Given an array A of positive integers, call a (contiguous, not necessarily distinct) subarray of A good if the number of different integers in that subarray is exactly K.

-- (For example, [1,2,3,1,2] has 3 different integers: 1, 2, and 3.)

-- Return the number of good subarrays of A.



-- Example 1:

-- Input: A = [1,2,1,2,3], K = 2
-- Output: 7
-- Explanation: Subarrays formed with exactly 2 different integers: [1,2], [2,1], [1,2], [2,3], [1,2,1], [2,1,2], [1,2,1,2].

-- Example 2:

-- Input: A = [1,2,1,3,4], K = 3
-- Output: 3
-- Explanation: Subarrays formed with exactly 3 different integers: [1,2,1,3], [2,1,3], [1,3,4].

module SubarraysKDiff where

import qualified Data.IntSet                        as Set


prefix :: [a] -> [[a]]
prefix [] = []
prefix xs = [xs] <> prefix (init xs)

suffix :: [a] -> [[a]]
suffix [] = []
suffix xs = [xs] <> suffix (tail xs)

sublists :: [a] -> [[a]]
sublists = concatMap suffix . prefix

subOfK :: [Int] -> Int -> [[Int]]
subOfK arr k =
    filter (\list -> Set.size (Set.fromList list) == k) $ sublists arr
