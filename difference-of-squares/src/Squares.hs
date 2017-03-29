module Squares (difference, squareOfSums, sumOfSquares) where

difference :: Integral a => a -> a
difference n = abs $ sumOfSquares n - squareOfSums n

squareOfSums :: Integral a => a -> a
squareOfSums n = (sum [1..n])^2

sumOfSquares :: Integral a => a -> a
sumOfSquares n = sum $ (\x -> x*x) <$> [1..n]
