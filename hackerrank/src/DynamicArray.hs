module DynamicArray where

import           Data.Bits
import           Data.Array.ST
import           Control.Monad.ST
import qualified Data.Vector.Mutable                        as MV
import qualified Data.Vector.Generic.Mutable as GM
import           Data.List                    hiding ( foldr )
import           Data.Text                           ( pack
                                                     , unpack
                                                     , stripStart
                                                     , stripEnd
                                                     )
import           System.Environment                  ( getEnv )
import qualified System.IO                          as IO

dynamicArray :: Int -> [[Int]] -> [Int]
dynamicArray n queries = undefined
  where
    vec n = runST $ do 
        v <- MV.new n
        forM_ [0..n] $ \i -> MV.write (MV.new 0) i
    seqs = foldr handleQuery (0, vec) queries
        handleQuery [q, x, y] (lastAnswer, v) = 
            let idx = (x `xor` lastAnswer) `mod` n
            in
                case q of
                    1 -> (lastAnswer, V.snoc (V.! v idx) y)
                    2 -> ()


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
