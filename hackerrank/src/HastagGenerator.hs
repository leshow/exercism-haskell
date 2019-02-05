module HashtagGenerator where

import           Data.Char

generateHashtag :: String -> Maybe String
generateHashtag s =
    let nospace = length $ filter (== ' ') s
    in  if nospace > 140 || nospace == 0
            then Nothing
            else
                Just
                $  "#"
                <> (concatMap (\(c : cs) -> toUpper c : cs) $ words s)
