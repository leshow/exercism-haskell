module RecursiveDigitSum where
{-
    superDigit(p) = superDigit(9875987598759875)
                  5+7+8+9+5+7+8+9+5+7+8+9+5+7+8+9 = 116
    superDigit(p) = superDigit(116)
                  1+1+6 = 8
    superDigit(p) = superDigit(8)
-}
import           Data.Monoid
import           Data.Coerce
import           Data.Char

superDigit :: String -> Int -> Int
superDigit n k = if length n > 1
    then superDigit (show $ k * getSum (foldMap (sum_ . digitToInt) n)) 1
    else digitToInt $ head n

sum_ :: Int -> Sum Int
sum_ = coerce

-- bleh
superDigit' :: String -> Int -> Int
superDigit' n k = if length n > 1
    then superDigit' (show $ k * sum (map digitToInt n)) 1
    else digitToInt $ head n
