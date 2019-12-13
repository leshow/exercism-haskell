module CollatzConjecture
  ( collatz
  )
where

import           Data.List                           ( unfoldr )

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0
  = Nothing
  | otherwise
  = Just
    . toInteger
    . length
    . unfoldr (\x -> if x == 1 then Nothing else Just (go x, go x))
    $ n
 where
  go x | even x    = x `div` 2
       | odd x     = 3 * x + 1
       | otherwise = 0
