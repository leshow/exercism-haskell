module Diamond
    ( diamond
    )
where

import           Data.Maybe                          ( fromJust )
import           Data.List                           ( intercalate )
import           Data.Char                           ( ord
                                                     , isUpper
                                                     )
type Diamond = [String]

diamond :: Char -> Maybe Diamond
diamond ch | isUpper ch = Just $ half <> tail (reverse half)
           | otherwise  = Nothing
  where
    i    = ord ch - ord 'A' + 1
    half = [ line i c | c <- ['A' .. ch] ]

line :: Int -> Char -> [Char]
line i c = side <> [c] <> middle <> side
  where
    sidecount = i - (ord c - ord 'A') - 1
    midcount  = 2 * i - 3 - 2 * sidecount
    side      = replicate sidecount ' '
    middle =
        if midcount == 0 || c == 'A' then "" else replicate midcount ' ' <> [c]

prettyPrint :: Diamond -> IO ()
prettyPrint s = putStrLn $ intercalate "\n" s

showDiamond :: Maybe Diamond -> IO ()
showDiamond = prettyPrint . fromJust

