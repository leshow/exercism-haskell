module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    )
where

data LinkedList a = N | (:-) a (LinkedList a) deriving (Eq, Show)
infixr 5 :-

datum :: LinkedList a -> a
datum (x :- _) = x
datum _        = error "Explode"

fromList :: [a] -> LinkedList a
fromList []       = N
fromList (x : xs) = x :- fromList xs

isNil :: LinkedList a -> Bool
isNil N = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new x xs = x :- xs

next :: LinkedList a -> LinkedList a
next (_ :- xs) = xs
next _         = error "Explode"

nil :: LinkedList a
nil = N

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = fromList . reverse . toList

toList :: LinkedList a -> [a]
toList N         = []
toList (x :- xs) = x : toList xs
