{-# LANGUAGE UndecidableInstances #-}
module HList where

import           Data.Kind
import           Data.Typeable
import           Data.Maybe
import           Data.Foldable

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

-- Eq instance from book
-- instance Eq (HList '[]) where
--     HNil == HNil = True

-- instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
--     (a :*: as) == (b :*: bs) = a == b && as == bs

-- Ord instance exercise:
-- instance Ord (HList '[]) where
--     HNil `compare` HNil = EQ

-- instance (Ord a, (Ord (HList as))) => Ord (HList (a ': as)) where
--     (a :*: as) `compare` (b :*: bs) = case a `compare` b of
--         EQ -> as `compare` bs
--         LT -> LT
--         GT -> GT

-- Show instance exercise:
-- instance Show (HList '[]) where
--     show HNil = "[]"

-- instance (Show a, Show (HList as)) => Show (HList (a ': as)) where
--     showsPrec p (a :*: as) =
--         showParen (p > 5) $ showsPrec 6 a . (":*:" <>) . showsPrec 5 as

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

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
    All c '[] = ()
    All c (t ': ts) = (c t, All c ts)

-- given this, we can write Eq, Ord, and Show more directly:

instance All Eq ts => Eq (HList ts) where
    HNil       == HNil       = True
    (a :*: as) == (b :*: bs) = a == b && as == bs

instance (All Ord ts, All Eq ts) => Ord (HList ts) where
    HNil       `compare` HNil       = EQ
    (a :*: as) `compare` (b :*: bs) = case a `compare` b of
        EQ -> as `compare` bs
        LT -> LT
        GT -> GT

instance All Show ts => Show (HList ts) where
    show HNil = "[]"
    showsPrec p (a :*: as) =
        showParen (p > 5) $ showsPrec 6 a . (":*:" <>) . showsPrec 5 as
-- Ex 7.1-ii
-- Removing the Show t constraint make the show instance invalid, because there is no way to
-- know that we can call 'show' on t


data HasShow where
    HasShow ::Show t => t -> HasShow

-- Ex 7.1-iii -- write show in terms of elimHasShow
instance Show HasShow where
    show s = "HasShow " <> elimHasShow show s

elimHasShow :: (forall a . Show a => a -> r) -> HasShow -> r
elimHasShow f (HasShow a) = f a

-- 7.1.1

data Dynamic where
    Dynamic ::Typeable t => t -> Dynamic

elimDynamic :: (forall a . Typeable a => a -> r) -> Dynamic -> r
elimDynamic f (Dynamic a) = f a

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

liftD2
    :: forall a b r
     . (Typeable a, Typeable b, Typeable r)
    => Dynamic
    -> Dynamic
    -> (a -> b -> r)
    -> Maybe Dynamic
liftD2 d1 d2 f = fmap Dynamic . f <$> fromDynamic @a d1 <*> fromDynamic @b d2

pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b = fromMaybe (error "Failed cast at runtime") $ asum
    [ liftD2 @String @String a b (<>)
    , liftD2 @String @Int a b $ \sa ib -> sa ++ show ib
    , liftD2 @Int @String a b $ \ia sb -> show ia ++ sb
    , liftD2 @Int @Int a b (+)
    ]

-- 7.1.2

data Has (c :: Type -> Constraint) where
    Has ::c t => t -> Has c

elimHas :: (forall a. c a => a -> r) -> Has c -> r
elimHas f (Has a) = f a

type HasSh = Has Show
type Dyn = Has Typeable
