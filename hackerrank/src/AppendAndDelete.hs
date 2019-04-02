module AppendAndDelete where

import           Data.Function                       ( (&) )

-- must be EXACTLY k changes
appendAndDelete :: String -> String -> Int -> String
appendAndDelete s t k | diff > k                  = "No"
                      | diff `mod` 2 == k `mod` 2 = "Yes"
                      | slen + tlen - k < 0       = "Yes"
                      | otherwise                 = "No"
  where
    common = zipWith (==) s t & takeWhile id & length
    slen   = length s
    tlen   = length t
    diff   = slen + tlen - 2 * common

{-
    the cases are:
        - difference is more than k. "abcde" "abc" k = 2 = "No"
            - below cases handle those with diff less than k
        - diff and k are both even or both odd "10101" "1010" k = 3 = "Yes"
        - "abc" "abc" 7 = delete all of s, then write it again, we can keep deleting from
            s when it's empty.
        - otherwise "No"
-}
