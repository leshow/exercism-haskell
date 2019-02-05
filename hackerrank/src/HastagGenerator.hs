module HashtagGenerator where

import           Data.Char
import           Control.Monad

generateHashtag :: String -> Maybe String
generateHashtag s =
    let nospace = length $ filter (/= ' ') s
        str     = concatMap (\(c : cs) -> toUpper c : cs) $ words s
    in  if nospace > 140 || nospace == 0 || null str
            then Nothing
            else Just $ '#' : str

generateHashtag' :: String -> Maybe String
generateHashtag' s = do
    let nospace = length $ filter (/= ' ') s
        str     = concatMap caps $ words s
    guard (nospace <= 140 || nospace /= 0 || not (null str))
    pure $ '#' : str
    where caps (c : cs) = toUpper c : cs
