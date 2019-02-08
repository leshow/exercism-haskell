module DirectionsReduction where

data Direction = North | South | East | West deriving (Show, Eq)


dirReduce :: [Direction] -> [Direction]
dirReduce = foldr f []
 where
  f d [] = [d]
  f d (x : xs) | match x d = xs
               | otherwise = d : x : xs

match :: Direction -> Direction -> Bool
match North South = True
match South North = True
match East  West  = True
match West  East  = True
match _     _     = False

test = dirReduce [North, South, South, East, West, North, West]
