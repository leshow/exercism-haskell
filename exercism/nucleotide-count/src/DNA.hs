module DNA (nucleotideCounts) where

import           Control.Monad (foldM)
import           Data.Map      (Map)
import qualified Data.Map      as M

nucleotideCounts :: String -> Either String (Map Char Integer)
nucleotideCounts xs = foldM (\m x -> do
            x' <- isValid x
            let n = M.insertWith (+) x' 1 m
            return $ n) M.empty xs
    where
        isValid x = case x of
            'A' -> Right x
            'T' -> Right x
            'G' -> Right x
            'C' -> Right x
            _   -> Left $ "Error"


-- with monadT
-- import           Control.Monad (foldM)
-- import           Data.Map      (Map)
-- import qualified Data.Map      as M

-- nucleotideCounts :: String -> Either String (Map Char Integer)
-- nucleotideCounts xs = runExceptT (execStateT (nucleotideCounts' xs) M.empty)

-- nucleotideCounts' :: String -> StateT (Map Char Integer) (ExceptT String []) ()
-- nucleotideCounts' xs = do
--     x <- (lift.lift) xs
--     unless (x `elem` "ATCG") (throwE "Error")
--     modify (M.insertWith (+) x 1)
