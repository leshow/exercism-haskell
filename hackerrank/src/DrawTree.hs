module DrawTree where

import           System.Random
import           Control.Monad
import           Data.Word
import           Graphics.X11.Turtle


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
