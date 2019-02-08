module DayOfProgrammer where

months :: [Int]
months = [31, 31, 30, 31, 30, 31, 31]

dayOfProgrammer :: Int -> String
dayOfProgrammer year
  | year > 1918 && fohunnid || fonothunnid = date (29 : months)
  | year < 1918 && fo                      = date (29 : months)
  | year == 1918                           = date (15 : months)
  | otherwise                              = date (28 : months)
  where
    date xs = show (day xs) <> ".09." <> show year
    day list = 256 - sum list
    fohunnid    = year `mod` 400 == 0
    fonothunnid = year `mod` 4 == 0 && year `mod` 100 /= 0
    fo          = year `mod` 4 == 0
