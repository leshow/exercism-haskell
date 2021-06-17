module SimpleLisp where

import           Control.Applicative
import           Data.Char
import           Data.List                           ( isPrefixOf
                                                     , sortOn
                                                     )
import           Text.ParserCombinators.ReadP

shuffle :: [a] -> Int -> [a]
shuffle xs n = concatMap (\(x, y) -> [x, y]) $ zip a b
        where (a, b) = splitAt n xs

goal :: String -> String
goal cmd | "G" `isPrefixOf` cmd    = "G" <> goal (drop 1 cmd)
         | "(al)" `isPrefixOf` cmd = "al" <> goal (drop 4 cmd)
         | "()" `isPrefixOf` cmd   = "o" <> goal (drop 2 cmd)
         | otherwise               = ""

-- Input: (let x 2 (mult x (let x 3 y 4 (add x y))))
-- Output: 14

data Expr = Mult Expr Expr
        | Add Expr Expr
        | Num Int
        | Let [(String, Expr)] Expr
        | Parens Expr
        deriving (Show)

runParser :: ReadP a -> String -> Maybe a
runParser parser input =
        let res = readP_to_S parser input
        in  if null res || snd (last res) /= ""
                    then Nothing
                    else Just $ fst (last res)
expr :: ReadP Expr
expr = add <|> mult <|> num <|> lets <|> parens

add :: ReadP Expr
add = do
        string "add" <* skipSpaces
        left  <- expr <* skipSpaces
        right <- expr <* skipSpaces
        pure $ Add left right

num :: ReadP Expr
num = do
        n <- munch1 isDigit <* skipSpaces
        pure $ Num (read n)

lets :: ReadP Expr
lets = do
        string "let" <* skipSpaces
        ids  <- sepBy1 ident skipSpaces
        rest <- expr <* skipSpaces
        pure $ Let ids rest

ident :: ReadP (String, Expr)
ident = do
        s <- munch1 isAlpha <* skipSpaces
        e <- expr <* skipSpaces
        pure (s, e)


parens :: ReadP Expr
parens = do
        char '(' <* skipSpaces
        e <- expr <* skipSpaces
        char ')' <* skipSpaces
        pure $ Parens e

mult :: ReadP Expr
mult = do
        string "mult" <* skipSpaces
        left  <- expr <* skipSpaces
        right <- expr <* skipSpaces
        pure $ Mult left right

sortSentence :: String -> String
sortSentence x =
        unwords
                $ map fst
                $ sortOn snd
                $ map
                          (\s ->
                                  ( takeWhile isAlpha s
                                  , read @Int $ dropWhile isAlpha s
                                  )
                          )
                $ words x
