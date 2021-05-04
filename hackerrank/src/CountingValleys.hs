module CountingValleys where
{-| 
Gary is an avid hiker. He tracks his hikes meticulously, paying close attention to small details like topography. During his last hike he took exactly steps. For every step he took, he noted if it was an uphill, , or a downhill, step. Gary's hikes start and end at sea level and each step up or down represents a
unit change in altitude. We define the following terms:

    A mountain is a sequence of consecutive steps above sea level, starting with a step up from sea level and ending with a step down to sea level.
    A valley is a sequence of consecutive steps below sea level, starting with a step down from sea level and ending with a step up to sea level.

Given Gary's sequence of up and down steps during his last hike, find and print the number of valleys he walked through.
For example, if Gary's path is s = [DDUUUUDD], he first enters a valley 2 units deep.
 Then he climbs out an up onto a mountain 2 units high. Finally, he returns to sea level and ends his hike.

# Function Description
Complete the countingValleys function in the editor below. It must return an integer that denotes the number of valleys Gary traversed.

countingValleys has the following parameter(s):

    n: the number of steps Gary takes
    s: a string describing his path

# Sample Input
8
UDDDUDUU

# Sample Output
1

# Explanation
If we represent _ as sea level, a step up as /, and a step down as \, Gary's hike can be drawn as:

_/\      _
   \    /
    \/\/

-}

countingValleys :: Int -> [Char] -> Int
countingValleys _ s = go s 0 0
  where
    go :: [Char] -> Int -> Int -> Int
    go [] _ !valleys = valleys
    go (x : xs) !level !valleys
        | x == 'U' && level + 1 == 0 = go xs (level + 1) (valleys + 1)
        | x == 'U'                   = go xs (level + 1) valleys
        | x == 'D'                   = go xs (level - 1) valleys
        | otherwise                  = error "only U and D allowed"

