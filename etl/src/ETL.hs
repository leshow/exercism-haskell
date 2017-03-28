module ETL (transform) where

import           Data.Char (toLower)
import           Data.Map  (Map)
import qualified Data.Map  as M


transform :: Map a String -> Map Char a
transform = M.fromList . concatMap (\(g, xs) ->
    let
        list = zip (fmap toLower xs) (repeat g)
    in
        list
    ) . M.toList
