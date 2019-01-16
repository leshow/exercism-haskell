module Parse where

import           Control.Monad
import           Data.Char
import           Control.Applicative
import           GHC.TypeLits
import           GHC.TypeNats
import           Data.Kind

data Parsed = Digit Integer | Hex Integer | Word String deriving Show

parseHex :: Parsed -> Char -> [Parsed]
parseHex (Hex i) c = if isHexDigit c
    then return (Hex ((i * 16) + toInteger (digitToInt c)))
    else mzero
parseHex _ _ = mzero

parseDigit :: Parsed -> Char -> [Parsed]
parseDigit (Digit i) c = if isDigit c
    then return (Digit ((i * 10) + toInteger (digitToInt c)))
    else mzero
parseDigit _ _ = mzero

parseWord :: Parsed -> Char -> [Parsed]
parseWord (Word s) c = if isAlpha c then return (Word (s <> [c])) else mzero
parseWord _        _ = mzero

parse :: Parsed -> Char -> [Parsed]
parse p c = parseHex p c <|> parseDigit p c <|> parseWord p c

parseArg :: String -> [Parsed]
parseArg s = do
    initial <- pure (Hex 0) <|> pure (Digit 0) <|> pure (Word "")
    foldM parse initial s
