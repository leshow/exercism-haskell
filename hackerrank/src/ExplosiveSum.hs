module ExplosiveSum where


parts :: (Ord a, Num a, Enum a) => a -> [[a]]
parts 0 = [[]]
parts n = [ m : p | m <- [1 .. n], p <- parts (n - m), null p || m <= head p ]
