module Bob (responseFor) where

import           Data.Char (isLower, isLower, isSpace, isUpper)
import           Data.List (dropWhileEnd)

responseFor :: String -> String
responseFor xs
    | all isSpace xs = "Fine. Be that way!"
    | any isUpper xs && not (any isLower xs) = "Whoa, chill out!"
    | last (dropWhileEnd isSpace xs) == '?' = "Sure."
    | otherwise = "Whatever."
