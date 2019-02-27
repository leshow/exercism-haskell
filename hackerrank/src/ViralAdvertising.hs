module ViralAdvertising where

-- turned this one into some pointfree golf
viralAdvertising :: Int -> Int
viralAdvertising =
    sum . flip take (map (`div` 2) $ iterate ((3 *) . (`div` 2)) 5)


