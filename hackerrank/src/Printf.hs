{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}
module Printf where

import           Data.Kind                           ( Type )
import           GHC.TypeLits
import           Control.Monad.State
import           System.Random
import qualified Control.Monad.Par                  as P
import qualified Control.Parallel.Strategies        as P


data (:<<) (a :: k1) (b :: k2)
infixr 5 :<<

class HasPrintf a where
    type Printf a :: Type

instance HasPrintf (a :: Symbol) where
    type Printf a = String

instance HasPrintf a => HasPrintf ((text :: Symbol) :<< a) where
    type Printf (text :<< a) = Printf a

instance HasPrintf a => HasPrintf ((nottext :: Type) :<< a) where
    type Printf (nottext :<< a) = nottext -> Printf a


rollDie :: State StdGen Int
rollDie = do
    gen <- get
    let (r, g) = randomR (1, 6) gen
    put g
    pure r

listDie :: State [Int] Int
listDie = do
    l <- get
    let nxt = if null l then 1 else head l + 1
    put (nxt : l)
    pure nxt

ran :: Int -> [Int] -> ([Int], [Int])
ran n = runState (replicateM n listDie)

randomNums :: Random a => Int -> Int -> ([a], StdGen)
randomNums m n = runState (replicateM n (state random)) (mkStdGen m)

tryPar :: (t -> b) -> (t, t) -> (b, b)
tryPar f (x, y) = P.runEval $ do
    a <- P.rpar (f x)
    b <- P.rpar (f y)
    _ <- P.rseq a
    _ <- P.rseq b
    pure (a, b)

parFib :: (Num a, Eq a) => a -> a
parFib 0 = 1
parFib 1 = 1
parFib n = P.runEval $ do
    n1 <- P.rpar $ parFib (n - 1)
    n2 <- P.rpar $ parFib (n - 2)
    pure (n1 + n2)

parMap :: (a -> b) -> [a] -> P.Eval [b]
parMap _ []       = pure []
parMap f (x : xs) = do
    x'  <- P.rpar (f x)
    xs' <- parMap f xs
    pure (x' : xs')

