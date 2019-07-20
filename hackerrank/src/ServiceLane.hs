module ServiceLane where

import           Data.Array
import           Data.List
import           System.IO
import           System.Environment

serviceLane :: [[Int]] -> Array Int Int -> [Int]
serviceLane cases arr =
    fmap (\[i, j] -> minimum [ arr ! idx | idx <- [(i + 1) .. (j + 1)] ]) cases

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray (n - 1)
    return (line : rest)

main :: IO ()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr   <- openFile stdout WriteMode

    ntTemp <- getLine
    let nt = words ntTemp

    let n  = read (head nt) :: Int

    let t  = read (nt !! 1) :: Int

    widthTemp <- getLine

    let width = Data.List.map (read :: String -> Int) . words $ widthTemp

    casesTemp <- readMultipleLinesAsStringArray t
    let cases  = map (map (read :: String -> Int) . words) casesTemp
    let arr    = array (1, n) $ zip [1 .. n] width
    let result = serviceLane cases arr

    hPutStrLn fptr $ intercalate "\n" $ map show result

    hFlush fptr
    hClose fptr
