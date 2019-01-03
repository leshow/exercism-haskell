module GenerateParentheses where

{-
 Given n pairs of parentheses, write a function to generate all combinations of well-formed parentheses.

For example, given n = 3, a solution set is:

[
  "((()))",
  "(()())",
  "(())()",
  "()(())",
  "()()()"
]

-}
generateParentheses :: Int -> [String]
generateParentheses 0 = [""]
generateParentheses n =
    [ "(" ++ x ++ ")" ++ y
    | m <- [0 .. n - 1]
    , x <- generateParentheses m
    , y <- generateParentheses (n - 1 - m)
    ]
    -- 0 <= m <= n - 1
    -- 0 <= n - 1 - m

