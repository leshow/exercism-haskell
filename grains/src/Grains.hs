module Grains (square, total) where

import           Data.Maybe (catMaybes)

square :: Integer -> Maybe Integer
square n
    | n < 1 || n > 64 = Nothing
    | otherwise = Just $ 2 ^ (n - 1)

total :: Integer
total = sum $ catMaybes $ square <$> [1..64]
