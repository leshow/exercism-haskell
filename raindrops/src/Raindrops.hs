module Raindrops (convert) where

import           Data.Foldable (fold)
import           Data.Maybe    (fromMaybe)

convert :: Int -> String
convert n = fromMaybe (show n) $ fold [calc "Pling" 3, calc "Plang" 5, calc "Plong" 7]
    where
        calc s f = if n `mod` f == 0 then Just s else Nothing
