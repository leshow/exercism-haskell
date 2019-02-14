module RunLength (decode, encode) where

import           Data.Char   (isDigit)
import           Data.Monoid ((<>))

decode :: String -> String
decode "" = ""
decode string = replicate n c <> decode other
    where
        d = takeWhile isDigit string
        (c:_) = dropWhile isDigit string
        n = if d == "" then 1 else read d
        other = drop (1 + length d) string



encode :: String -> String
encode "" = ""
encode (x:xs) = char <> [x] <> encode other
    where
        len = length (takeWhile (==x) xs)
        char = if len == 0 then "" else show (len + 1)
        other = drop len xs
