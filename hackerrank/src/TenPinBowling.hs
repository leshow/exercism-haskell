module TenPinBowling where

import           Data.Char                           ( digitToInt )

bowlingScore :: String -> Int
bowlingScore = score . go
  where
    go []             = []
    go (' '     : xs) = go xs
    go ('X'     : xs) = 10 : go xs
    go (x : '/' : xs) = digitToInt x : 10 - digitToInt x : go xs
    go (x       : xs) = digitToInt x : go xs
    score []               = 0
    score (x     : y : []) = x + y
    score (x : y : z : []) = x + y + z
    score (x : y : z : xs) | x == 10     = x + y + z + score (y : z : xs)
                           | x + y == 10 = x + y + z + score (z : xs)
                           | otherwise   = x + y + score (z : xs)
