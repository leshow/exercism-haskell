module ValidParenthesis where

import           Data.Map                            ( Map )
import qualified Data.Map                           as Map
import           Data.Maybe                          ( isJust )
import           Control.Monad
import           Debug.Trace
{-
Given a string containing just the characters '(', ')', '{', '}', '[' and ']', determine if the input string is valid.

An input string is valid if:

    Open brackets must be closed by the same type of brackets.
    Open brackets must be closed in the correct order.

Note that an empty string is also considered valid.

Example 1:

Input: "()"
Output: true

Example 2:

Input: "()[]{}"
Output: true

Example 3:

Input: "(]"
Output: false

Example 4:

Input: "([)]"
Output: false

Example 5:

Input: "{[]}"
Output: true
-}
type Stack a = [a]

matching :: Map Char Char
matching = Map.fromList [(')', '('), (']', '['), ('}', '{')]

isValid :: String -> Bool
isValid s = go [] s
  where
    go :: Stack Char -> String -> Bool
    go []       ""         = True
    go _        ""         = False
    go []       (top : cs) = go [top] cs
    go (b : bs) (top : cs) = case Map.lookup top matching of
        Just c  -> (b == c) && go bs cs
        Nothing -> go (top : b : bs) cs

-- monadic parsing (with trace to inspect)
parse :: String -> Maybe String
parse xs@(')' : _ ) = return xs
parse xs@(']' : _ ) = return xs
parse xs@('}' : _ ) = return xs
parse (   '(' : xs) = do
    ')' : ys <- parse xs
    parse ys
parse ('[' : xs) = do
    ']' : ys <- parse xs
    parse ys
parse ('{' : xs) = do
    '}' : ys <- parse xs
    parse ys
parse (_ : xs) = parse xs
parse []       = return []

paren :: String -> Bool
paren xs = isJust $ do
    ys <- parse xs
    guard (null ys)


-- w/ trace to inspect control flow
parse' :: String -> Maybe String
parse' xs@(')' : _ ) = return xs
parse' xs@(']' : _ ) = return xs
parse' xs@('}' : _ ) = return xs
parse' (   '(' : xs) = do
    ')' : ys <- trace ("found ( in " ++ show xs) (parse' xs)
    trace ("found ) in " ++ show ys) (parse' ys)
parse' ('[' : xs) = do
    ']' : ys <- trace ("found [ in " ++ show xs) (parse' xs)
    trace ("found ] in " ++ show ys) (parse' ys)
parse' ('{' : xs) = do
    '}' : ys <- trace ("found { in " ++ show xs) (parse' xs)
    trace ("found } in " ++ show ys) (parse' ys)
parse' (_ : xs) = parse' xs
parse' []       = return []
