module Fibonacci where

import           Prelude                      hiding ( Monad )

fib :: Integer -> Integer
fib n | n < 0     = -go !! abs (fromIntegral n)
      | otherwise = go !! fromIntegral n
    where go = 0 : 1 : zipWith (+) go (tail go)

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

data State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
    return a = State $ \s -> (a, s)
    sg >>= f = State $ \s -> let (a, s') = runState sg s in runState (f a) s'

data Reader s a = Reader { runReader :: s -> a }

instance Monad (Reader s) where
    return a = Reader $ const a
    (Reader r) >>= f = Reader $ \s -> runReader (f (r s)) s
