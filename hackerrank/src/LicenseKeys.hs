module LicenseKeys where

import           Data.List                           ( intercalate )

licenseKeys :: Int -> String -> String
licenseKeys k = intercalate "-" . splitN k . filter (/= '-')

splitN :: Int -> [a] -> [[a]]
splitN _ [] = []
splitN n xs = take n xs : splitN n (drop n xs)

