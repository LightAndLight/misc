{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler.Syntax where

import Compiler.Kind (Kind)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Vector (Vector)

newtype Module
  = Module (Vector Definition)
  deriving (Show)

data TypeOrKind = Type Type | Kind Kind
  deriving (Show, Eq)

data Pattern
  = PVar (Maybe Text) TypeOrKind
  | PUnit
  deriving (Show)

data Definition
  = -- | @f (x : T) : T = e@
    Binding Text (Vector Pattern) Type Expr
  deriving (Show)

data Type
  = -- | @a@
    TVar Var
  | -- | @_@
    TUnknown
  | -- | @forall (a : k). T@
    TForall Text Kind Type
  | -- | @exists (a : k). T@
    TExists Text Kind Type
  | -- | @T -> U@
    TArrow Type Type
  | -- | @{ f_0 : T_0, f_1 : T_1, ..., f_n : T_n }@
    TRecord (Vector (Text, Type))
  | -- | @()@
    TUnit
  | -- | @bool@
    TBool
  | -- | @i32@
    TI32
  | -- | @i64@
    TI64
  | -- | @Array a@
    TArray Type
  deriving (Show, Eq)

data Argument = ArgValue Expr | ArgType Type
  deriving (Show, Eq)

data IsValueOrType = IsValue | IsType
  deriving (Show, Eq)

-- | A unique variable
newtype Unique = U Word
  deriving (Show, Eq)
  deriving newtype (Hashable)

data Var
  = Src Text
  | Gen Unique
  deriving (Show, Eq)

data Expr
  = -- | @x@
    Var Var
  | -- | @let x [: T] = v in e@
    Let Text (Maybe Type) Expr Expr
  | -- | @x : T@
    Ann Expr Type
  | -- | @\\x -> e@
    Lam IsValueOrType Text Expr
  | -- | @a b@
    App Expr Argument
  | -- | @{ f_0 = e_0, f_1 = e_1, ..., f_n = e_n }@
    Record (Vector (Text, Expr))
  | -- | @e.field@
    Proj Expr Text
  | -- | @true@, @false@
    Bool Bool
  | -- | @99@
    Integer Integer
  | -- | @[x, y, z]@
    Array (Vector Expr)
  deriving (Show, Eq)
