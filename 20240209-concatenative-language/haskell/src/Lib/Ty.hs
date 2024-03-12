module Lib.Ty where

import Data.Kind (Type)

data Ty
  = TString
  | TInt
  | TChar
  | TBool
  | TSum Ty Ty
  | TProd Ty Ty
  | TExp Ty Ty
  | TList Ty
  | TMaybe Ty

data Ctx = Nil | (:.) Ctx Ty

infixl 5 :.

data Index :: Ctx -> Ty -> Type where
  Z :: Index (ctx :. a) a
  S :: Index ctx a -> Index (ctx :. b) a

data STy (ty :: Ty) where
  STInt :: STy TInt
  STBool :: STy TBool
  STChar :: STy TChar
  STString :: STy TString

data SCtx (ctx :: Ctx) where
  SNil :: SCtx Nil
  SSnoc :: SCtx ctx -> STy ty -> SCtx (ctx :. ty)