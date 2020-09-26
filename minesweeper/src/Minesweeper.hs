module Minesweeper (annotate) where

import           Data.Char (intToDigit)

type Board = [String]

type Pos = (Int, Int)

annotate :: Board -> Board
annotate [] = []
annotate [""] = [""]
annotate b = countCell (0, 0) "" b

countCell :: Pos -> String -> Board -> Board
countCell (x, y) new board
  | x == rows && y == cols = lines (reverse new)
  | y == cols = countCell (x + 1, 0) ('\n':new) board
  | x == rows = countCell (x, y + 1) new board
  | otherwise = countCell (x, y + 1) (cell:new) board
  where
    rows = length board

    cols = length (head board)

    cell
      | board !! x !! y == '*' = '*'
      | otherwise = if val == 0
                    then ' '
                    else intToDigit val

    val = addNum (x, y) board

addNum :: Pos -> Board -> Int
addNum (x, y) b = add (x - 1, y)
  + add (x + 1, y)
  + add (x, y - 1)
  + add (x, y + 1)
  + add (x + 1, y - 1)
  + add (x - 1, y + 1)
  + add (x - 1, y - 1)
  + add (x + 1, y + 1)
  where
    add (x, y)
      | x < 0 = 0
      | y < 0 = 0
      | x >= length b = 0
      | y >= length (head b) = 0
      | otherwise = if b !! x !! y == '*'
                    then 1
                    else 0