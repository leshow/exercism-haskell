module Pangram
    ( isPangram
    )
where

import qualified Data.Set                           as Set
import           Data.Char                           ( toLower
                                                     , isAlpha
                                                     )
alphas :: [Char]
alphas = ['a' .. 'z']

isPangram :: String -> Bool
isPangram text = setmap alphas == setmap (toLower <$> filter isAlpha text)
    where setmap = foldr Set.insert Set.empty
