module School (School, add, empty, grade, sorted) where

import           Control.Arrow (second)

import qualified Data.Map      as M
import qualified Data.Set      as S

type Grade = Int
type Student = String
type School = M.Map Grade (S.Set Student)

add :: Grade -> Student -> School -> School
add g s = M.insertWith S.union g (S.singleton s)

empty :: School
empty = M.empty

grade :: Grade -> School -> [Student]
grade g = S.toAscList . M.findWithDefault S.empty g

sorted :: School -> [(Grade, [Student])]
sorted m = second S.toAscList <$> M.toAscList m
