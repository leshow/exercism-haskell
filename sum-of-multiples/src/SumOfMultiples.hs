module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ filter (\x -> any (\a -> x `mod` a == 0) factors) [0..limit-1]
