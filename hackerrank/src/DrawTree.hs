module DrawTree where

import           System.Random
import           Control.Monad
import           Data.Word
import           Graphics.X11.Turtle
import           Control.Monad.ST
import           Data.STRef
import           Data.Array
import           Data.Matrix                  hiding ( trace )
import           Debug.Trace
<<<<<<< HEAD
import           Data.Char                           ( ord )

=======
>>>>>>> Change speed to fastest

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

-- | Array passed to it must be 1-indexed because Data.Matrix is 1-indexed
-- ex. knapsack (listArray (1,3) [Item{weight=10,value=60}, Item{weight=20, value=100}, Item{weight=30, value=120}]) 50
knapsack :: Array Int Item -> Int -> Int
knapsack arr total = runST $ do
    let itemlen = 1 + snd (bounds arr)
        dp      = zero (itemlen + 1) (total + 1)
    traceShowM dp
    dref <- newSTRef dp
    forM_ [1 .. itemlen] $ \i -> forM_ [1 .. total] $ \j -> if
        | i == 1 || j == 1 -> do
            dp' <- readSTRef dref
            writeSTRef dref (setElem 0 (i, j) dp')
        | weight (arr Data.Array.! (i - 1)) <= j -> do
            let Item { weight = w, value = v } = arr Data.Array.! (i - 1)
            dp' <- readSTRef dref
            let w' = if j - w == 0 then 1 else j - w -- 1 indexes are stupid
            let
                maxV = max (v + dp' Data.Matrix.! (i - 1, w'))
                           (dp' Data.Matrix.! (i - 1, j))
            writeSTRef dref (setElem maxV (i, j) dp')
        | otherwise -> do
            dp' <- readSTRef dref
            let val = dp' Data.Matrix.! (i - 1, j)
            writeSTRef dref (setElem val (i, j) dp')
    dp' <- readSTRef dref
    traceShowM dp'
    pure (dp' Data.Matrix.! (itemlen, total))

koch :: Double -> Double -> Turtle -> IO ()
koch a order t
    | order <= 0 = forward t a
    | otherwise = forM_ [60, -120, 60, 0] $ \angle -> do
        koch (a / 3) (order - 1) t
        left t angle

runKoch :: IO ()
runKoch = do
    t <- openField >>= newTurtle
    let size  = 400
        order = 4
    penup t
    backward t (size / 1.732)
    left t 30
    pendown t
    speed t "fast"
    bgcolor t (0 :: Word8, 0 :: Word8, 0 :: Word8)
    pencolor t (201 :: Word8, 253 :: Word8, 255 :: Word8)
    beginfill t
    forM_ [(0 :: Word8) .. 2] $ \_ -> do
        koch size order t
        right t 120
    endfill t


minCoins :: Int -> [Int] -> Int
minCoins total coins
    | total <= 0
    = 0
    | otherwise
    = let possibleCoins = [ c | c <- coins, c <= total ]
      in  foldr
              (\c minValue -> min (1 + minCoins (total - c) coins) minValue)
              (maxBound @Int)
              possibleCoins

