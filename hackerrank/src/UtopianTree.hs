module UtopianTree where

import           Data.Foldable                       ( foldl' )


utopianTree :: Int -> Int
utopianTree = foldl' (flip ($)) 1 . flip take (cycle [(* 2), (+ 1)])
