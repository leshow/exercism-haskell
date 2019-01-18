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
isPangram =
    (Set.fromList alphas ==) . Set.fromList . fmap toLower . filter isAlpha
