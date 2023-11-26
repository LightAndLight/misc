{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Map.CommutativeMonoid (Union (..)) where

import Data.Map (Map)
import qualified Data.Map
import Data.Monoid.Commutative (CommutativeMonoid (..))
import Data.Semigroup.Commutative (CommutativeSemigroup (..))

newtype Union k v = Union (Map k v)

instance (Ord k, Semigroup v) => Semigroup (Union k v) where
  Union a <> Union b = Union (Data.Map.unionWith (<>) a b)

instance (Ord k, CommutativeSemigroup v) => CommutativeSemigroup (Union k v)

instance (Ord k, Semigroup v) => Monoid (Union k v) where
  mempty = Union mempty

instance (Ord k, CommutativeSemigroup v) => CommutativeMonoid (Union k v)