module Permutations where

import qualified Data.Set                           as Set
import           Data.List                           ( nub
                                                     , delete
                                                     )

perms :: Ord a => [a] -> [[a]]
perms = Set.toList . Set.fromList . perms_

perms_ :: [a] -> [[a]]
perms_ []       = pure []
perms_ (x : xs) = do
    x' <- perms_ xs
    ins x x'

ins :: a -> [a] -> [[a]]
ins x []       = [[x]]
ins x (y : ys) = [x : y : ys] <> map (y :) (ins x ys)


-- alternate 

permutations :: String -> [String]
permutations "" = [[]]
permutations xs =
    nub [ y | x <- xs, y <- map (x :) . permutations $ delete x xs ]
