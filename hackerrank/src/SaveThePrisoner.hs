module SaveThePrisoner where

saveThePrisoner :: Int -> Int -> Int -> Int
saveThePrisoner n m s = (drop (s - 1) [1 .. n] <> cycle [1 .. n]) !! (m - 1)

--without lists:

save' :: Int -> Int -> Int -> Int
save' n m s = ((m + s - 2) `mod` n) + 1
