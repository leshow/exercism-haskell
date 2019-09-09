module Phone
    ( number
    )
where

import           Data.Char
import           Control.Monad

number :: String -> Maybe String
number xs = do
    let nums = filter isDigit xs
    let len  = length nums
    guard (len == 10 || head nums == '1')
    join $ pure $ if len == 11
        then do
            let (c, a : _, e : _) = getLong nums
            let cnum              = read c :: Int
            guard (cnum `notElem` [2 .. 9])
            guard (a `elem` ['2' .. '9'])
            guard (e `elem` ['2' .. '9'])
            pure (tail nums)
        else do
            let (a : _, e : _) = getShort nums
            guard (a `elem` ['2' .. '9'])
            guard (e `elem` ['2' .. '9'])
            pure nums
  where
    getLong s = (take 1 s, take 3 (drop 1 s), take 3 (drop 4 s))
    getShort s = (take 3 s, take 3 (drop 3 s))
