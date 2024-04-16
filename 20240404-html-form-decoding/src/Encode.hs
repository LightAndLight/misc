{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
module Encode where

import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Map
import Data.Foldable (foldl')
import Data.Functor.Identity (Identity(..))
import Prelude hiding (map)
import Data.Functor.Compose (Compose (..))
import Barbies (FunctorB (..))
import Data.Functor.Contravariant.Divisible (Divisible, divided)
import Data.Functor.Contravariant (contramap, Contravariant, Op(..))
import Data.Map.Monoidal (MonoidalMap(..))

newtype Encoder a = Encoder { encode :: a -> Map String (NonEmpty String) }
  deriving (Contravariant) via Op (Map String (NonEmpty String))
  deriving (Divisible) via Op (MonoidalMap String (NonEmpty String))

type SimpleEncoder a = a -> String

string :: SimpleEncoder String
string value =
  value

int :: SimpleEncoder Int
int value =
  show value

simple :: String -> SimpleEncoder a -> Encoder a
simple key enc =
  Encoder $ \input ->
  Map.singleton key (pure $ enc input)

compound :: String -> Encoder a -> Encoder a
compound key enc =
  Encoder $ \value ->
  Map.mapKeys (\k -> key <> "." <> k) $ encode enc value

list ::
  Encoder a ->
  Encoder [a]
list enc =
  Encoder $ \input ->
  let len = length input in
  foldl'
    (\acc a -> Map.unionWith (<>) acc (Map.mapKeys (\k -> "values[]." <> k) $ encode enc a))
    (Map.singleton "length" (pure $ show len))
    input
