module ArrayManipulation where

{-|
Starting with a 1-indexed array of zeros and a list of operations, for each operation add a value to each of the array element between two given indices, inclusive. Once all operations have been performed, return the maximum value in your array.

For example, the length of your array of zeros

. Your list of queries is as follows:

    a b k
    1 5 3
    4 8 7
    6 9 1

Add the values of
between the indices and

inclusive:

index->	 1 2 3  4  5 6 7 8 9 10
	[0,0,0, 0, 0,0,0,0,0, 0]
	[3,3,3, 3, 3,0,0,0,0, 0]
	[3,3,3,10,10,7,7,7,0, 0]
	[3,3,3,10,10,8,8,8,1, 0]

The largest value is 10 after all operations are performed. 
-}
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
