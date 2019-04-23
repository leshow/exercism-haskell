{-# LANGUAGE UndecidableInstances #-}
module Printf where

import           Data.Kind                           ( Type )
import           Data.Monoid                         ( (<>) )
import           Data.Proxy                          ( Proxy(..) )
import           GHC.TypeLits

data (a :: k1) :<< (b :: k2)
infixr 5 :<<

class HasPrintf a where
    type Printf a :: Type

instance HasPrintf (text :: Symbol) where
    type Printf text = String

instance HasPrintf a => HasPrintf ((text :: Symbol) :<< a) where
    type Printf (text :<< a) = Printf a

instance HasPrintf a => HasPrintf ((param :: Type) :<< a) where
    type Printf (param :<< a) = param -> Printf a
