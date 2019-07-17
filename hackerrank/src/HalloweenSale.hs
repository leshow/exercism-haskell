module HalloweenSale where

-- Complete the howManyGames function below.
howManyGames :: Int -> Int -> Int -> Int -> Int
howManyGames p d m s =
    length
            (  takeWhile (/= (-1)) -- kinda dirty
            $  scanl
                   (\total price ->
                       if total - price >= 0 then total - price else (-1)
                   )
                   s
            $  [p, (p - d) .. m]
            ++ repeat m
            )
        - 1
