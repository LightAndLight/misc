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

data Index :: [Ty] -> Ty -> Type where
  Z :: Index (a ': ctx) a
  S :: Index ctx a -> Index (b ': ctx) a

data STy (ty :: Ty) where
  STInt :: STy TInt
  STBool :: STy TBool
  STChar :: STy TChar
  STString :: STy TString
  STProd :: STy a -> STy b -> STy (TProd a b)

data SList (ctx :: [Ty]) where
  SNil :: SList '[]
  SCons :: STy ty -> SList ctx -> SList (ty ': ctx)