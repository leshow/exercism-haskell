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
