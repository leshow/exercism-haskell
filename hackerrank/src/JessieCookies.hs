module JessieCookies where

import           Data.Heap                           ( Heap )
import qualified Data.Heap                          as Heap
--
-- Complete the cookies function below.
--

cookies :: Int -> [Int] -> Int
cookies k xs = go $ Heap.fromList xs
  where
    go :: Heap Int -> Int
    go heap = case Heap.uncons heap of
        Just (x, h') -> if x < k
            then case Heap.uncons h' of
                Just (y, h'') -> 1 + go (Heap.insert (x + y * 2) h'')
                Nothing       -> -1
            else 0
        Nothing -> -1
