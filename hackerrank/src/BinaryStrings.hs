module BinaryStrings where

binaryStrings :: String -> [String]
binaryStrings s = go s ""
 where
  go [] ys = [reverse ys]
  go (x : xs) ys | x == '?'  = go xs ('1' : ys) <> go xs ('0' : ys)
                 | otherwise = go xs (x : ys)

