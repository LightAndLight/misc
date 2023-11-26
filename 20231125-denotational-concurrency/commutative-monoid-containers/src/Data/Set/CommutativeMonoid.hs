{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Set.CommutativeMonoid (Union (..)) where

import Data.Monoid.Commutative (CommutativeMonoid (..))
import Data.Semigroup.Commutative (CommutativeSemigroup (..))
import Data.Set (Set)

newtype Union a = Union (Set a)
  deriving (Semigroup, Monoid)

instance (Ord a) => CommutativeSemigroup (Union a)
instance (Ord a) => CommutativeMonoid (Union a)