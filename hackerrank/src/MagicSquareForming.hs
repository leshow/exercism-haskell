module MagicSquare where

import           Data.List                           ( transpose )
import           Data.Function                       ( (&) )

type Square = [[Int]]

square :: Square
square = [[8, 1, 6], [3, 5, 7], [4, 9, 2]]

rotate :: Square -> Square
rotate = map reverse . transpose

rotations :: Square -> [Square]
rotations = iterate rotate

allSquares :: [Square]
allSquares = take 4 (rotations square) <> take 4 (rotations (transpose square))

cost :: Square -> Square -> Int
cost a b = zipWith (-) (concat a) (concat b) & map abs & sum

-- Complete the formingMagicSquare function below.
formingMagicSquare :: Square -> Int
formingMagicSquare s = minimum $ cost s <$> allSquares
