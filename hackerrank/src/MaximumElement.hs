module MaximumElement where

import           Control.Monad

data Node a = Node { el :: a, curmax :: a }

maxEl :: [Int] -> [Int]
maxEl = go [] (minBound :: Int)
  where
    go :: (Num a, Bounded a, Ord a) => [Node a] -> a -> [a] -> [a]
    go _ _ [] = []
    go s m (x : xs)
        | x == 3 = curmax (head s) : go s m xs
        | x == 2 = case s of
            []       -> go [] minBound xs
            [_     ] -> go [] minBound xs
            (y : ys) -> go ys (curmax y) xs
        | otherwise = let m' = max x m in go (node x m' : s) m' xs

node :: a -> a -> Node a
node a b = Node { el = a, curmax = b }

main :: IO ()
main = do
    nt <- getLine
    let n = read @Int nt
    args <- forM [1 .. n] $ \_ -> do
        l <- getLine
        let l' = read @Int <$> words l
        pure $ if head l' == 1 then last l' else head l'
    forM_ (maxEl args) print

