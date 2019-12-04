module Day3 where

import qualified Data.Map.Strict                    as Map
import           Data.Map.Strict                     ( Map )
import           Data.Function                       ( (&) )
import           Data.List.Split

data Dir = L | R | U | D deriving stock (Eq, Show, Read, Enum)

data Move = Move Dir Int deriving stock (Eq, Show)

dirX :: Dir -> Int
dirX L = -1
dirX R = 1
dirX U = 0
dirX D = 0

dirY :: Dir -> Int
dirY L = 0
dirY R = 0
dirY U = 1
dirY D = -1

readInput = do
    [one, two] <- lines <$> readFile "./day3_1.txt"
    let move1 = map move (splitOn "," one)
    let move2 = map move (splitOn "," two)
    pure move1

move :: String -> Move
move (d : xs) = Move (read [d] :: Dir) (read xs :: Int)
move _        = error "Parse failed-- dont do this in prod"

-- getPos :: [Move] -> Int -> Int -> Map (Int, Int) Int -> Map (Int, Int) Int
-- getPos (move: moves) a b m = 


get :: (Dir -> Int) -> Move -> [Int]
get f (Move d n) = moves where moves = map (\_ -> f d) [1 .. n]
