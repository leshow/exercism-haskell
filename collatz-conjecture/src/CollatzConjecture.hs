module CollatzConjecture
    ( collatz
    )
where

collatz :: Integer -> Maybe Integer
collatz n | n <= 0    = Nothing
          | otherwise = Just $ go n
  where
    go :: Integer -> Integer
    go n | n == 1    = 0
         | even n    = 1 + go (n `div` 2)
         | odd n     = 1 + go (3 * n + 1)
         | otherwise = 0
