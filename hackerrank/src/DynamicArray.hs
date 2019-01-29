module DynamicArray where

import           Data.Bits
import           Data.Array.ST
import           Control.Monad.ST
import           Control.Monad
import           Data.STRef
import qualified Data.Vector                        as V
import           Data.Vector                         ( (!) )
import           Data.List                    hiding ( foldr )
import           Data.Text                           ( pack
                                                     , unpack
                                                     , stripStart
                                                     , stripEnd
                                                     )
import           System.Environment                  ( getEnv )
import qualified System.IO                          as IO

dynamicArray :: Int -> [[Int]] -> [Int]
dynamicArray n queries = runST $ do
    m <- newArray (1, n + 1) V.empty :: ST s (STArray s Int (V.Vector Int))
    res <- newSTRef V.empty
    lastAnswer <- newSTRef 0
    forM_ queries $ \[q, x, y] -> do
        last <- readSTRef lastAnswer
        let idx = ((x `xor` last) `mod` n) + 1
        case q of
            1 -> do
                v' <- readArray m idx
                writeArray m idx (V.snoc v' y)
            2 -> do
                v' <- readArray m idx
                let newlast = (v' ! (y `mod` (V.length v')))
                writeSTRef lastAnswer newlast
                r' <- readSTRef res
                writeSTRef res (V.snoc r' newlast)
    V.toList <$> readSTRef res



lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray (n - 1)
    return (line : rest)

main :: IO ()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr   <- IO.openFile stdout IO.WriteMode

    nqTemp <- getLine
    let nq = Data.List.words $ rstrip nqTemp

    let n  = read (nq !! 0) :: Int

    let q  = read (nq !! 1) :: Int

    queriesTemp <- readMultipleLinesAsStringArray q
    let queries = Data.List.map
            (Data.List.map (read :: String -> Int) . Data.List.words . rstrip)
            queriesTemp

    let result = dynamicArray n queries

    IO.hPutStrLn fptr $ Data.List.intercalate "\n" $ Data.List.map show result

    IO.hFlush fptr
    IO.hClose fptr
