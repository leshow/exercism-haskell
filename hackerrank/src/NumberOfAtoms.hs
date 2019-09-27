module NumberOfAtoms where

import           Text.Parsec                  hiding ( (<|>) )
import           Text.Parsec.String                  ( Parser )
import           Control.Applicative          hiding ( many )
import           Control.Monad
import           Data.Maybe                          ( fromMaybe )
import           Text.Read                           ( readMaybe )
import qualified Data.Map.Strict                    as Map
import           Data.STRef
import           Control.Monad.ST

parsye rule = parse rule ""

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

data Formula = Atom String Int | Comb [Formula] Int
    deriving (Eq, Show)

-- Mg(OH)2
-- Comb Mg 1 [Atom O 1, Atom H 1] 2
-- "K4(ON(SO3)2)2"
-- Comb K 4 [ Atom O 1, Comb N 1 [ Atom S 1, Atom O 3] 2 ] 2
-- Mg(OH)2

parseFormula :: String -> IO ()
parseFormula = print . regularParse formula

formula :: Parser Formula
formula = do
    formulas <- many1 (atom <|> comb)
    pure $ Comb formulas 1

atom :: Parser Formula
atom = do
    name   <- choice (try <$> elements)
    amount <- many digit
    pure $ Atom name (fromMaybe 1 (readMaybe amount))

comb :: Parser Formula
comb = do
    void $ char '('
    name <- many1 (atom <|> comb)
    void $ char ')'
    amount <- many1 digit
    pure $ Comb name (fromMaybe 1 (readMaybe amount))

elements =
    string
        <$> words
                "Np Pu Am Cm Bk Cf Es Fm Md No Lr Rf Db Sg Bh Hs Mt Ds Rg Cn Nh Fl Mc Lv Ts Og He Li Be Ne Na Mg Al Si Cl Ar Ca Sc Ti Cr Mn Fe Co Ni Cu Zn Ga Ge As Se Br Kr Rb Sr Zr Nb Mo Tc Ru Rh Pd Ag Cd In Sn Sb Te Xe Cs Ba La Ce Pr Nd Pm Sm Gd Tb Dy Ho Er Tm Yb Lu Hf Ta Re Os Ir Pt Au Hg Tl Pb Bi Po At Ra Ac Th Pa H B C N O F P S K V Y I W U"

addTotalST :: Formula -> [(String, Int)]
addTotalST f = Map.toList $ go f Map.empty
  where
    go :: Formula -> Map.Map String Int -> Map.Map String Int
    go (Atom s  i) m = Map.insertWith (*) s i m
    go (Comb xs i) m = runST $ do
        let maps = fmap (`go` m) xs
        acc <- newSTRef Map.empty
        forM_
            maps
            (\a -> forM_
                (Map.toList a)
                (\(k, v) -> do
                    acc' <- readSTRef acc
                    writeSTRef acc (Map.insertWith (*) k (i * v) acc')
                )
            )
        readSTRef acc

addTotal :: Formula -> [(String, Int)]
addTotal f = Map.toList $ go f Map.empty
  where
    go :: Formula -> Map.Map String Int -> Map.Map String Int
    go (Atom s  i) m = Map.insertWith (*) s i m
    go (Comb xs i) m = foldr
        (flip $ Map.foldrWithKey (\k v x -> Map.insertWith (*) k (i * v) x))
        Map.empty
        (fmap (`go` m) xs)
