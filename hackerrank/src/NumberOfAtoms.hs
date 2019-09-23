module NumberOfAtoms where

import           Text.Parsec                  hiding ( (<|>) )
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.String                  ( Parser )
import           Control.Applicative          hiding ( many )
import           Control.Monad                hiding ( many )
import           Data.Char
import qualified Data.IntSet                        as Set

parsye rule text = parse rule "" text

brackets :: Parser String
brackets = do
    void $ lexeme $ char '('
    e <- lexeme $ many1 digit
    void $ lexeme $ char ')'
    pure (read e)

lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    whitespace
    pure x

whitespace :: Parser ()
whitespace = void $ many $ oneOf "\n\t"

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

parseWithLeftOver :: Parser a -> String -> Either ParseError (a, String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
    where leftOver = manyTill anyToken eof

data Formula = Atom String Int | Comb String Int [Formula] Int
    deriving stock (Eq, Show)

-- Mg(OH)2
-- Comb Mg 1 [Atom O 1, Atom H 1] 2
-- "K4(ON(SO3)2)2"
-- Comb K 4 [ Atom O 1, Comb N 1 [ Atom S 1, Atom O 3] 2 ] 2
-- Mg(OH)2
caps = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

atom :: Parser Formula
atom = do
    el <- oneOf caps
    e  <- try (many1 digit <|> many1 letter)
    pure $ Atom el e

data AtomSuffix = Digit Int | Append String
    deriving stock (Eq, Show)

parseSuffix :: Parser AtomSuffix
parseSuffix = do
    p <- try (many1 digit <|> many1 letter)
    readMaybe
