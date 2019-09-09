module Phone
    ( number
    )
where

import           Data.Char
import           Control.Monad

number :: String -> Maybe String
number xs =
    let
        nums = filter isNumber xs
        len  = length nums
    in
        do
            guard (null clean || not anyLetters)
            guard allowedSymbols
            guard (len <= 11)
            guard (len /= 9)
            join $ pure $ if len == 11
                then
                    let (c, a : _, e : _) = getLong nums
                        cnum              = read c :: Int
                    in  do
                            guard (cnum `notElem` [0, 2, 3, 4, 5, 6, 7, 8, 9])
                            guard (a /= '0' && a /= '1' && e /= '0' && e /= '1')
                            pure (tail nums)
                else
                    let (a : _, e : _) = getShort nums
                    in  do
                            guard (a /= '0' && a /= '1' && e /= '0' && e /= '1')
                            pure nums
  where
    anyLetters = any (\c -> isLetter c && c /= ' ') clean
    allowedSymbols =
        null clean
            || any
                   (\c ->
                       c /= ')' || c /= '(' || c /= '+' || c /= '.' || c /= ' '
                   )
                   clean
    getLong s = (take 1 s, take 3 (drop 1 s), take 3 (drop 4 s))
    getShort s = (take 3 s, take 3 (drop 3 s))
    clean = filter (not . isNumber) xs
