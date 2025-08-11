{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Syntax where

import Codec.Serialise (Serialise)
import Data.Binary (Binary (..))
import qualified Data.Binary as Binary
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

instance Binary a => Binary (Vector a) where
  get = do
    len <- Binary.get @Int
    Vector.replicateM len Binary.get

  put xs = do
    Binary.put (Vector.length xs)
    traverse_ Binary.put xs

newtype Program = Program (Vector Definition)
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

data Definition
  = -- |
    -- @
    -- {name}({args}) :- {body} where {bindings}.
    -- @
    Rule
      -- | Name
      !Text
      -- | Arguments
      !(Vector Text)
      -- | Body
      !(Vector Relation)
      -- | Bindings
      !(Vector Binding)
  | Fact
      -- | Name
      !Text
      -- | Arguments
      !(Vector Constant)
  deriving (Show, Eq)

data Relation
  = Relation !Text !(Vector Expr)
  deriving (Show, Eq)

data Expr
  = Wild
  | Var !Text
  | Constant !Constant
  | Map
      -- | Keys must match exactly
      Bool
      (Map Constant Expr)
  | List (Vector Expr)
  deriving (Show, Eq)

data Binding
  = -- | @{name} is {bexpr}
    Binding
      -- | Name
      !Text
      -- | Value
      !BExpr
  deriving (Show, Eq)

data BExpr
  = BKeys Expr
  | BItems Expr
  deriving (Show, Eq)

data Constant
  = CBool !Bool
  | CString !Text
  | CNatural !Natural
  | CMap !(Map Constant Constant)
  | CList !(Vector Constant)
  deriving (Show, Eq, Ord, Generic, Serialise, Binary)

formatConstant :: Constant -> Lazy.Text
formatConstant (CString s) =
  fromString (show s)
formatConstant (CNatural n) =
  fromString (show n)
formatConstant (CBool b) =
  if b then fromString "true" else fromString "false"
formatConstant (CList xs) =
  fromString "["
    <> Lazy.intercalate (fromString ", ") (formatConstant <$> Vector.toList xs)
    <> fromString "]"
formatConstant (CMap xs) =
  fromString "["
    <> Lazy.intercalate
      (fromString ", ")
      ((\(k, v) -> formatConstant k <> fromString " = " <> formatConstant v) <$> Map.toList xs)
    <> fromString "]"
