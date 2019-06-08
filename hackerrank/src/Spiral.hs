module Spiral where

import           Graphics.X11.Turtle
import           Data.Word

drawSpiral :: Double -> Turtle -> IO ()
drawSpiral len t
    | len <= 0 = pure ()
    | otherwise = do
        forward t len
        right t 90
        drawSpiral (len - 5) t

runSpiral :: IO ()
runSpiral = openField >>= newTurtle >>= drawSpiral 100

type Triangle = (Point, Point, Point)
type Point = (Double, Double)

triangle :: Triangle
triangle = ((-150, -100), (0, 150), (150, -100))

magenta, cyan, red, green, blue, white :: (Word8, Word8, Word8)
magenta = (255, 51, 255)
cyan = (0, 255, 255)
red = (255, 0, 0)
green = (0, 255, 0)
blue = (0, 0, 255)
white = (0, 0, 0)

colors :: [(Word8, Word8, Word8)]
colors = [magenta, cyan, white, red, green, blue]

drawTriangle :: ColorClass c => Triangle -> c -> Turtle -> IO ()
drawTriangle ((a, a'), (b, b'), (c, c')) col t = do
    pencolor t col
    penup t
    goto t a a'
    pendown t
    beginfill t
    goto t b b'
    goto t c c'
    goto t a a'
    endfill t


sierpenski :: Triangle -> Double -> Turtle -> IO ()
sierpenski pts@((a, a'), (b, b'), (c, c')) deg t = do
    drawTriangle pts (colors !! (round deg)) t
    if deg > 0
        then do
            sierpenski ((a, a'), mid (a, a') (b, b'), mid (a, a') (c, c'))
                       (deg - 1)
                       t
            sierpenski ((b, b'), mid (a, a') (b, b'), mid (b, b') (c, c'))
                       (deg - 1)
                       t
            sierpenski ((c, c'), mid (c, c') (b, b'), mid (a, a') (c, c'))
                       (deg - 1)
                       t
            pure ()
        else pure ()
  where
    mid :: Point -> Point -> Point
    mid (aa, aa') (bb, bb') = ((aa + bb) / 2, (aa' + bb') / 2)

runSierpenski :: IO ()
runSierpenski = openField >>= newTurtle >>= sierpenski triangle 4
