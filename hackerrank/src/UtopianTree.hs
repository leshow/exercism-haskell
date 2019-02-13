module UtopianTree where

import           Data.Foldable                       ( foldl' )


utopianTree :: Int -> Int
utopianTree n = foldl' (flip ($)) 1 $ take n (cycle [(* 2), (+ 1)])
