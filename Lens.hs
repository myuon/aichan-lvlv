{-# LANGUAGE Rank2Types, DeriveFunctor #-}
module Lens where
import Data.Functor.Identity
import Data.Functor.Constant
import Control.Monad.State
import Control.Applicative

type Lens s t a b = Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s `fmap` afb (sa s)

(^.) :: s -> Lens' s a -> a
s ^. l = getConstant $ l Constant s

(.~) :: Lens s t a b -> b -> s -> t
l .~ b = runIdentity . l (\_ -> Identity b)

(%~) :: Lens s t a b -> (a -> b) -> s -> t
l %~ f = runIdentity . l (Identity . f)

infixl 5 &
(&) :: a -> (a -> b) -> b
(&) = flip ($)

use :: (Functor m, Monad m) => Lens' s a -> StateT s m a
use l = (^.l) <$> get

infixl 4 .=,%=,+=,-=
(.=) :: (Monad m) => Lens' s a -> a -> StateT s m ()
l .= x = modify (l .~ x)

(%=) :: (Monad m) => Lens' s a -> (a -> a) -> StateT s m ()
l %= f = modify (l %~ f)

(+=) :: (Monad m, Num a) => Lens' s a -> a -> StateT s m ()
l += x = l %= (+x)

(-=) :: (Monad m, Num a) => Lens' s a -> a -> StateT s m ()
l -= x = l %= (subtract x)

