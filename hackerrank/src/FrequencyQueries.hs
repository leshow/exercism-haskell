module FrequencyQueries where

import           Data.Foldable
import qualified Data.Map.Strict                    as Map
{-
You are given queries. Each query is of the form two integers described below:
- 1:x Insert x in your data structure.
- 2:y Delete one occurence of y from your data structure, if present.
- 3:z Check if any integer is present whose frequency is exactly z. If yes, print 1 else 0.
-}
-- Complete the freqQuery function below.
freqQuery :: [[Int]] -> [Int]
freqQuery = reverse . fst . foldl' op ([], Map.empty) 
  where
    op (acc, m) ([1, x]) = (acc, Map.insertWith (+) x 1 m)
    op (acc, m) ([2, y]) = (acc, Map.update clear y m)
    op (acc, m) ([3, z]) = if (z `elem` m) then (1 : acc, m) else (0 : acc, m)
    clear f = if (f - 1) == 0 then Nothing else Just $ f - 1
