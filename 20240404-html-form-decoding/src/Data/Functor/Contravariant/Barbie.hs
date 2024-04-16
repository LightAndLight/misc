module Data.Functor.Contravariant.Barbie where

import Data.Functor.Barbie (FunctorB)
import Data.Functor.Contravariant.Divisible (Divisible)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Contravariant (contramap)

class FunctorB shape => ContraversableB shape where
  bcontraverse :: Divisible e => (forall x. f x -> e (g x)) -> shape f -> e (shape g) 

bconsequence :: (ContraversableB shape, Divisible f) => shape (Compose f g) -> f (shape g)
bconsequence = bcontraverse getCompose

bconsequence' :: (ContraversableB shape, Divisible f) => shape f -> f (shape Identity)
bconsequence' = bcontraverse (contramap runIdentity)
