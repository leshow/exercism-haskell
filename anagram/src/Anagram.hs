module Anagram
    ( anagramsFor
    )
where

import qualified Data.Map.Strict                    as M
import           Data.Char                           ( toLower )

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = fst
    <$> filter (uncurry $ cmp xs s) (zip xss (fmap count xss))
  where
    count :: String -> M.Map Char Integer
    count = foldr (\c -> M.insertWith (+) (toLower c) 1) M.empty

    s :: M.Map Char Integer
    s = count xs

    cmp :: Eq a => String -> a -> String -> a -> Bool
    cmp a b x y = b == y && (toLower <$> a) /= (toLower <$> x)


