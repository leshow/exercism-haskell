module Diamond
    ( diamond
    )
where

import           Data.Maybe                          ( fromJust )
import           Data.List                           ( intercalate )
import           Data.Char                           ( ord
                                                     , chr
                                                     , isUpper
                                                     )

diamond :: Char -> Maybe [String]
diamond c | isUpper c = Just $ go num 0 num
          | otherwise = Nothing
  where
    num = ord c - ord 'A'
    tochar x = chr (ord 'A' + x)
    go i ends 0 = [line (tochar i) ends 0]
    go i ends middle =
        line (tochar i) ends middle : go i (ends - 1) (middle + 2)



line :: Char -> Int -> Int -> String
line c ends 0      = replicate ends '.' <> [c] <> replicate ends '.'
line c 0    middle = [c] <> replicate middle '.' <> [c]
line c ends middle =
    replicate ends '.' <> [c] <> replicate middle '.' <> [c] <> replicate
        ends
        '.'
  where


type Diamond = [String]

prettyPrint :: Diamond -> IO ()
prettyPrint s = putStrLn $ intercalate "\n" s

showDiamond :: Maybe Diamond -> IO ()
showDiamond = prettyPrint . fromJust

-- let half = go (ord c) n 0
--       in  if length half > 1
--               then Just $ reverse half <> tail half
--               else Just half
