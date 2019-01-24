module RecursiveStaircase where


import           Control.Monad.ST
import           Data.Array.ST
import           Control.Monad

stepPerms :: Int -> Int
stepPerms n
    | n == 0 = 0
    | n == 1 = 1
    | n == 2 = 2
    | n == 3 = 4
    | otherwise = runST $ do
        arr <- newArray (1, n + 1) 0 :: ST s (STArray s Int Int)
        writeArray arr 1 1
        writeArray arr 2 1
        writeArray arr 3 2
        writeArray arr 4 4
        forM_ [4 .. (n + 1)] $ \i -> do
            a   <- readArray arr (i - 1)
            a'  <- readArray arr (i - 2)
            a'' <- readArray arr (i - 3)
            writeArray arr i (a + a' + a'')
        readArray arr (n + 1)
