module StringsMix where

import           Data.Map                            ( Map )
import qualified Data.Map                           as Map
import           Data.List                           ( sortBy )
import           Data.Char                           ( isLower )
import           Debug.Trace                         ( traceShow )
import           Data.Function                       ( (&) )
import           Data.Maybe                          ( catMaybes )

mix :: String -> String -> String
mix s1 s2 = go ll1 ll2
  where
    byLengths s = sortBy (\(_, a) (_, b) -> compare b a) s
    greaterThanOne = filter (\(_, l) -> l > 1)
    l1             = Map.fromList $ lens s1
    l2             = Map.fromList $ lens s2
    ll1            = byLengths $ getUniqueLargest l1 l2
    ll2            = byLengths $ getUniqueLargest l2 l1
    lens           = greaterThanOne . Map.toList . charmap . filter isLower
    go []           []             = ""
    go []           ((c2, l2) : _) = "2:" <> replicate l2 c2
    go ((c, l) : _) []             = "1:" <> replicate l c
    go x@((c, l) : xs) y@((c2, l2) : ys)
        | l == l2 && c == c2 = "=:" <> replicate l c <> "/" <> go xs ys
        | l == l2 = if c > c2
            then "1:" <> replicate l c <> "/" <> go xs y
            else "2:" <> replicate l2 c2 <> "/" <> go x ys
        | l > l2 = "1:" <> replicate l c <> "/" <> go xs y
        | l < l2 = "2:" <> replicate l2 c2 <> "/" <> go x ys

charmap :: String -> Map Char Int
charmap = foldr (\c -> Map.insertWith (+) c 1) Map.empty

getUniqueLargest :: Map Char Int -> Map Char Int -> [(Char, Int)]
getUniqueLargest m1 m2 =
    Map.toList m1
        & foldr
              (\(c, l) acc -> case Map.lookup c m2 of
                  Just l2 -> case compare l l2 of
                      LT -> acc
                      EQ -> (c, l) : acc
                      GT -> (c, l) : acc
                  Nothing -> (c, l) : acc
              )
              []
