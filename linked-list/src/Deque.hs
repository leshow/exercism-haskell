{-# LANGUAGE NamedFieldPuns #-}

module Deque
    ( Deque
    , mkDeque
    , pop
    , push
    , shift
    , unshift
    )
where

import           Data.IORef
import           Control.Monad

data Deque a = Deque {front :: IORef [a], back :: IORef [a] }

mkDeque :: IO (Deque a)
mkDeque = liftM2 Deque (newIORef []) (newIORef [])

pop :: Deque a -> IO (Maybe a)
pop Deque { front, back } = do
    b <- readIORef back
    case b of
        (x : xs) -> writeIORef back xs >> pure (Just x)
        _        -> do
            f <- readIORef front
            case reverse f of
                (y : ys) -> writeIORef front ys >> pure (Just y)
                _        -> pure Nothing

push :: Deque a -> a -> IO ()
push Deque { back } v = do
    b <- readIORef back
    writeIORef back (v : b)

unshift :: Deque a -> a -> IO ()
unshift Deque { front } x = do
    f <- readIORef front
    writeIORef front (x : f)

-- written like this for fun.
shift :: Deque a -> IO (Maybe a)
shift Deque { front, back } = readIORef front >>= \case
    (x : xs) -> writeIORef front xs >> pure (Just x)
    _        -> reverse <$> readIORef back >>= \case
        (y : ys) -> writeIORef back ys >> pure (Just y)
        _        -> pure Nothing
