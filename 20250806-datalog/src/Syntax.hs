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
import Data.Aeson (ToJSON (..))
import qualified Data.Aeson as Json
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key

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
    -- {name}({args}) :- {body} where {local_bindings}.
    -- @
    Rule
      -- | Name
      !Text
      -- | Arguments
      !(Vector Text)
      -- | Body
      !(Vector Relation)
      -- | Local bindings
      !(Vector LocalBinding)

    -- |
    -- @
    -- {name} = {bexpr}.
    -- @
  | Binding
      -- | Name
      !Text
      -- | Value
      !BExpr
  | Fact
      -- | Name
      !Text
      -- | Arguments
      !(Vector Constant)
  deriving (Show, Eq)

data BExpr
  = BAggregate
      -- | Aggregation function
      !Text
      -- | Collection to aggregate
      !Stream
  deriving (Show, Eq)

data Stream
  -- | @[ {item} | {source} ]@
  = Stream
      -- | Item
      !(Vector Expr)
      -- | Source
      !Relation
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
      !Bool
      (Map Constant Expr)
  | List !(Vector Expr)
  deriving (Show, Eq)

data LocalBinding
  = -- | @{name} is {l_bexpr}
    LocalBinding
      -- | Name
      !Text
      -- | Value
      !LBExpr
  deriving (Show, Eq)

data LBExpr
  = LBKeys Expr
  | LBItems Expr
  deriving (Show, Eq)

data Constant
  = CBool !Bool
  | CString !Text
  | CNatural !Natural
  | CMap !(Map Constant Constant)
  | CList !(Vector Constant)
  deriving (Show, Eq, Ord, Generic, Serialise, Binary)

instance ToJSON Constant where
  toJSON (CBool b) = toJSON b
  toJSON (CString s) = toJSON s
  toJSON (CNatural n) = toJSON n
  toJSON (CMap m) =
    maybe
      (toJSON $ Map.toList m)
      (Json.Object . KeyMap.fromList)
      (traverse
        (\(k, v) -> case k of CString k' -> Just (Key.fromText k', toJSON v); _ -> Nothing)
        (Map.toList m)
      )
  toJSON (CList xs) = toJSON xs

toConstant :: Map Text Constant -> Expr -> Maybe Constant
toConstant _ Wild = Nothing
toConstant subst (Var v) = Map.lookup v subst
toConstant _subst (Constant c) = Just c
toConstant subst (Map True m) = CMap <$> traverse (toConstant subst) m
toConstant _ (Map False _) = Nothing
toConstant subst (List xs) = CList <$> traverse (toConstant subst) xs

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
