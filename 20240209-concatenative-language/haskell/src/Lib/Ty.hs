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
  STSum :: STy a -> STy b -> STy (TSum a b)
  STExp :: STy a -> STy b -> STy (TExp a b)
  STMaybe :: STy a -> STy (TMaybe a)
  STList :: STy a -> STy (TList a)

class KnownTy (ty :: Ty) where
  tyVal :: STy ty

instance KnownTy TInt where tyVal = STInt

instance KnownTy TString where tyVal = STString

instance KnownTy TBool where tyVal = STBool

instance KnownTy TChar where tyVal = STChar

instance (KnownTy a, KnownTy b) => KnownTy (TProd a b) where tyVal = STProd tyVal tyVal

instance (KnownTy a, KnownTy b) => KnownTy (TSum a b) where tyVal = STSum tyVal tyVal

instance (KnownTy a, KnownTy b) => KnownTy (TExp a b) where tyVal = STExp tyVal tyVal

instance (KnownTy a) => KnownTy (TMaybe a) where tyVal = STMaybe tyVal

instance (KnownTy a) => KnownTy (TList a) where tyVal = STList tyVal

data SList (ctx :: [Ty]) where
  SNil :: SList '[]
  SCons :: STy ty -> SList ctx -> SList (ty ': ctx)

class KnownCtx (ctx :: [Ty]) where
  ctxVal :: SList ctx

instance KnownCtx '[] where
  ctxVal = SNil

instance (KnownTy a, KnownCtx as) => KnownCtx (a ': as) where
  ctxVal = SCons tyVal ctxVal