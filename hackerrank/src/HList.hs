module HList where

import           Data.Kind

data HList (a :: [Type]) where
    HNil ::HList '[]
    (:*:) ::a -> HList as -> HList (a ': as)
infixr 5 :*:

hLen :: HList as -> Int
hLen HNil       = 0
hLen (_ :*: xs) = 1 + hLen xs

hHead :: HList (a ': as) -> a
hHead (t :*: _) = t

testList :: HList '[Maybe Bool, Int] -> Int
testList (_ :*: t' :*: HNil) = t'

-- Ord instance from book
instance Eq (HList '[]) where
    HNil == HNil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
    (a :*: as) == (b :*: bs) = a == b && as == bs

-- Ord instance exercise:
instance Ord (HList '[]) where
    HNil `compare` HNil = EQ

instance (Ord a, (Ord (HList as))) => Ord (HList (a ': as)) where
    (a :*: as) `compare` (b :*: bs) = case a `compare` b of
        EQ -> as `compare` bs
        LT -> LT
        GT -> GT

-- Show instance exercise:
instance Show (HList '[]) where
    show HNil = "[]"

instance (Show a, Show (HList as)) => Show (HList (a ': as)) where
    showsPrec p (a :*: as) = showParen (p > 5) $ showsPrec 6 a . (":*:"<>) . showsPrec 5 as

-- for reference:
data List a = N | C a (List a)

instance Eq a => Eq (List a) where
    N        == N        = True
    N        == (C _ _)  = False
    (C _ _ ) == N        = False
    (C a as) == (C b bs) = a == b && as == bs

instance Ord a => Ord (List a) where
    compare N        N        = EQ
    compare N        (C _ _)  = LT
    compare (C _ _ ) N        = GT
    compare (C a as) (C b bs) = case a `compare` b of
        EQ -> as `compare` bs
        LT -> LT
        GT -> GT

