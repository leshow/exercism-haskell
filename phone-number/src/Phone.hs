module Phone (number) where

import           Data.Char (isAlpha, isDigit, isSpace)

number :: String -> Maybe String
number xs
    | fmap (==10) len == Just True = nums
    | fmap (==11) len == Just True && y == '1' = tail <$> nums
    | otherwise = Nothing
    where
        nums = filter isDigit <$> traverse doChar xs
        (Just y) = head <$> nums
        len = length <$> nums

doChar :: Char -> Maybe Char
doChar c
    | isDigit c = Just c
    | isSpace c || c == '.' || c == '(' || c == ')' || c == '-' = Just ' '
    | isAlpha c = Nothing
    | otherwise = Nothing
