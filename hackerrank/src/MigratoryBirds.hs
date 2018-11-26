module MigratoryBirds where

{-
You have been asked to help study the population of birds migrating across the continent. Each type of bird you are interested in will be identified by an integer value. Each time a particular kind of bird is spotted, its id number will be added to your array of sightings. You would like to be able to find out which type of bird is most common given a list of sightings. Your task is to print the type number of that bird and if two or more types of birds are equally common, choose the type with the smallest ID number.

For example, assume your bird sightings are of types arr = {1,1,2,2,3}
. There are two each of types and 1, 2 and one sighting of type . Pick the lower of the two types seen twice: type . 

-}


import qualified Data.IntMap                        as Map
import           Data.IntMap                         ( IntMap )
import           Data.Lens
import           Data.Foldable

migratoryBirds :: [Int] -> Maybe Int
migratoryBirds arr = maximumOf traverse (intmap arr)
    where intmap = foldl' (\acc c -> Map.insertWith @Int (+) c 1 acc) Map.empty
