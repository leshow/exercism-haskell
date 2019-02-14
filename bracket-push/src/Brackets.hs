module Brackets
    ( arePaired
    )
where

import           Control.Monad
import           Data.Maybe
--import qualified Data.Map as Map

arePaired :: String -> Bool
arePaired xs = isJust $ do
    ys <- parse xs
    guard (null ys)

parse :: String -> Maybe String
parse xs@(')' : _ ) = pure xs
parse xs@('}' : _ ) = pure xs
parse xs@(']' : _ ) = pure xs
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
parse []       = pure []


{-
anotherWay :: String -> Bool
anotherWay = go []
  where
    go []       ""       = True
    go _        ""       = False
    go []       (c : cs) = go [c] cs
    go (b : bs) (c : cs) = case Map.lookup c matching of
        Just x  -> b == x && go bs cs
        Nothing -> go (c : b : bs) cs

-}
