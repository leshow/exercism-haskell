module SumByFactors where

import           Data.List
import qualified Data.Set                           as Set
import           Data.Monoid                         ( (<>) )

sumOfDivided :: [Integer] -> [(Integer, Integer)]
sumOfDivided xs =
    map (\f -> (f, sum $ filter (\x -> x `mod` f == 0) xs))
        $ sort
        $ Set.toList
        $ Set.fromList
        $ concatMap (primeFactors . abs) xs


primeFactors :: Integer -> [Integer]
primeFactors n = case factors of
    [] -> [n]
    _  -> factors <> primeFactors (div n (head factors))
  where
    factors =
        take 1
            . filter (\x -> mod n x == 0)
            $ [2 .. round $ sqrt $ fromIntegral n]
