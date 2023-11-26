module Data.Monoid.Commutative (CommutativeMonoid (..)) where

import Data.Monoid (Product, Sum)
import Data.Semigroup.Commutative (CommutativeSemigroup (..))

class (CommutativeSemigroup m, Monoid m) => CommutativeMonoid m

instance (Num a) => CommutativeMonoid (Sum a)
instance (Num a) => CommutativeMonoid (Product a)