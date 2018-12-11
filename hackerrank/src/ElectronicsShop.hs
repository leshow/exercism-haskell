module ElectronicsShop where

import           Data.Maybe                          ( fromMaybe )
import           Control.Monad
{-|
Monica wants to buy a keyboard and a USB drive from her favorite electronics store.
The store has several models of each. Monica wants to spend as much as possible for the items, 
given her budget.

Given the price lists for the store's keyboards and USB drives, and Monica's budget, find and
print the amount of money Monica will spend. If she doesn't have enough money to both a 
keyboard and a USB drive, print -1 instead. She will buy only the two required items.

Sample Input 0
10 2 3
3 1
5 2 8

Sample Output 0
9

Explanation 0
She can buy the 2nd
keyboard and the 3rd USB drive for a total cost of 8 + 1 = 9.
-}

getMoney :: [Int] -> [Int] -> Int -> [Int]
getMoney keyboards drives b =
    [ val | kb <- keyboards, drv <- drives, let val = kb + drv, val <= b ]

getMoneySpent :: [Int] -> [Int] -> Int -> Int
getMoneySpent keyboards drives b = fromMaybe (-1) mostExp
  where
    passing = getMoney keyboards drives b
    mostExp = safeMaximum passing

safeMaximum :: Ord a => [a] -> Maybe a
safeMaximum xs | null xs   = Nothing
               | otherwise = Just $ maximum xs
