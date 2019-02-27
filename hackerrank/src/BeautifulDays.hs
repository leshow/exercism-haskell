module BeautifulDays where

-- Complete the beautifulDays function below.
beautifulDays :: Int -> Int -> Int -> Int
beautifulDays i j k =
    length [ n | n <- [i .. j], (n - reverseNum n) `mod` k == 0 ]

reverseNum :: Int -> Int
reverseNum i = go i 0
  where
    go !i !n | i == 0    = n
             | otherwise = go (i `div` 10) (n * 10 + i `mod` 10)
