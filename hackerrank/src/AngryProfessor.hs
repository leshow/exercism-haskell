module AngryProfessor where

angryProfessor :: Int -> [Int] -> Bool
angryProfessor = flip ((<) . length . filter (<= 0))
