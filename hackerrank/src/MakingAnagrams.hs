module MakingAnagrams where

import qualified Data.Map                           as Map

makeAnagram :: String -> String -> Int
makeAnagram a b = sumdiff (Map.differenceWith getdiff am bm)
    + sumdiff (Map.difference bm am)
  where
    am      = charmap a
    bm      = charmap b
    sumdiff = Map.foldl' (+) 0
    charmap = foldr (\c -> Map.insertWith (+) c 1) Map.empty
    getdiff a b | a == b = Nothing
                | a > b  = Just (a - b)
                | a < b  = Just (b - a)
