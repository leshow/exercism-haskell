module DrawTree where

import           System.Random
import           Control.Monad
import           Data.Word
import           Graphics.X11.Turtle
import           Control.Monad.ST
import           Data.STRef
import           Data.Array
import           Data.Matrix
import           Debug.Trace

drawTree :: Double -> Double -> Turtle -> IO ()
drawTree len sz t
    | len <= 5 = pure ()
    | otherwise = do
        -- new values
        angle <- randomRIO (20, 40)
        nlen  <- randomRIO (10, 15)
        let size = sz - 2
        if size <= 0 then pensize t 1 else pensize t size
        pencolor t ((139, 69, 19) :: (Word8, Word8, Word8))
        forward t len
        right t angle
        drawTree (len - nlen) size t
        left t (angle + angle)
        drawTree (len - nlen) size t
        right t angle
        pencolor t ((0, 255, 0) :: (Word8, Word8, Word8))
        backward t len

runTree :: IO ()
runTree = do
    f <- openField
    t <- newTurtle f
    left t 90
    penup t
    backward t 100
    pendown t
    pencolor t ((139, 69, 19) :: (Word8, Word8, Word8))
    pensize t 10
    drawTree 75 10 t


data Item = Item
    { weight :: Int
    , value :: Int
    } deriving (Show, Eq)

knapsack :: Array Int Item -> Int -> Int
knapsack arr total = runST $ do
    let itemlen = snd $ bounds arr
        dp      = matrix (itemlen + 1) (total + 1) $ \(_, _) -> 0
    dref <- newSTRef dp
    forM_ [0 .. itemlen] $ \i -> do
        forM_ [0 .. total] $ \j -> do
            if
                | i == 0 || j == 0 -> do
                    dp' <- readSTRef dref
                    writeSTRef dref (setElem 0 (i, j) dp')
                | weight (arr Data.Array.! (i - 1)) <= j -> do
                    let Item { weight = w, value = v } =
                            arr Data.Array.! (i - 1)
                    dp' <- readSTRef dref
                    let maxV = max (v + dp' Data.Matrix.! (i - 1, j - w))
                                   (dp' Data.Matrix.! (i - 1, j))
                    writeSTRef dref (setElem maxV (i, j) dp')
                | otherwise -> do
                    dp' <- readSTRef dref
                    let val = dp' Data.Matrix.! (i - 1, j)
                    writeSTRef dref (setElem val (i, j) dp')
    dp' <- readSTRef dref
    pure (dp' Data.Matrix.! (itemlen, total))
