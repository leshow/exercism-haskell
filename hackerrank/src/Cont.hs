module Cont where

newtype Cont a = Cont { unCont :: forall r. (a -> r) -> r }

instance Functor Cont where
    fmap f ca = Cont $ \cb ->
        ca `unCont` \a ->
        cb (f a)

instance Applicative Cont where
    pure :: a -> Cont a
    pure a = Cont $ \cb -> cb a
    (<*>) :: Cont (a -> b) -> Cont a -> Cont b
    cf <*> ca = Cont $ \cb -> 
        cf `unCont` \f ->
        ca `unCont` \a ->
        cb (f a)

instance Monad Cont where
    return = pure
    (>>=) :: Cont a -> (a -> Cont b) -> Cont b
    ca >>= f = Cont $ \cb -> 
        ca `unCont` \a -> 
        f a `unCont` \b -> 
        cb b
