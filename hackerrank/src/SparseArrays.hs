module SparseArrays where

import           Data.Map                            ( Map )
import qualified Data.Map                           as Map
import           Data.Traversable                    ( forM )
import           Data.Maybe                          ( fromJust )


matchingStrings :: [String] -> [String] -> [Int]
matchingStrings strings queries = fromJust doquery
 where
  stringMap :: Map String Int
  stringMap = foldr (\s acc -> Map.insertWith (+) s 1 acc) Map.empty strings
  doquery   = forM queries $ \query -> case Map.lookup query stringMap of
    Just count -> Just count
    Nothing    -> Just 0
