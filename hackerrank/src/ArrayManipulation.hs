module ArrayManipulation where

import           Data.Array.ST
import           Control.Monad.ST
import           Control.Monad
import           Data.STRef


arrayManipulation :: Int -> [[Int]] -> Int
arrayManipulation n queries = runST $ do
    res <- newSTRef (minBound :: Int)
    arr <- newArray (1, n + 1) 0 :: ST s (STArray s Int Int)
    forM_ queries $ \[a, b, k] -> do
        a' <- readArray arr a
        writeArray arr a (a' + k)
        b' <- readArray arr (b + 1)
        writeArray arr (b + 1) (b' - k)
    lsum <- newSTRef @Int 0
    forM_ [1 .. n + 1] $ \i -> do
        cur <- readArray arr i
        modifySTRef' lsum (+ cur)
        s' <- readSTRef lsum
        m' <- readSTRef res
        if s' > m' then writeSTRef res s' else pure ()
    readSTRef res
