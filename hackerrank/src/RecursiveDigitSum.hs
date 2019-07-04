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
import           Data.Foldable

superDigit :: Int -> Int -> Int
superDigit n k =
    flip getSuper 0 $ getSum $ fold $ (coerce @Int . read) <$> replicate
        k
        (show n)

getSuper :: Int -> Int -> Int
getSuper !num !acc | acc < 10 && num == 0 = acc
                   | num == 0 = getSuper acc 0
                   | otherwise = getSuper (num `div` 10) (num `mod` 10 + acc)
