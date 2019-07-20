module ChocolateFeast where

chocolateFeast :: Int -> Int -> Int -> Int
chocolateFeast n c m = go n 0 0
  where
    go money !wrappers !total
        | money < c && wrappers < m
        = total
        | otherwise
        = let bars      = money `div` c
              wrappers_ = wrappers + bars
              n_        = money `mod` c
          in  if wrappers_ >= m
                  then
                      let extra = wrappers_ `div` m
                      in  go n_
                             (extra + (wrappers_ `mod` m))
                             (total + bars + extra)
                  else go n_ wrappers_ total + bars
