{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Compiler.Plugin.Interface (Quoted (..), Expr (..), Index (..), Ctx (..), getCtx, quote) where

import Data.Kind (Type)
import GHC.Exts (noinline)
import GHC.Stack (HasCallStack)

data Expr :: [Type] -> Type -> Type where
  Var :: forall ctx a. Index ctx a -> Expr ctx a
  Lam :: forall ctx a b. Expr (a ': ctx) b -> Expr ctx (a -> b)
  App :: forall ctx a b. Expr ctx (a -> b) -> Expr ctx a -> Expr ctx b
  Int :: forall ctx. Int -> Expr ctx Int
  Add :: forall ctx. Expr ctx Int -> Expr ctx Int -> Expr ctx Int
  IfThenElse :: forall ctx a. Expr ctx Bool -> Expr ctx a -> Expr ctx a -> Expr ctx a
  Bool :: forall ctx. Bool -> Expr ctx Bool

deriving instance Show (Expr ctx a)

data Index :: [Type] -> Type -> Type where
  Z :: forall ctx a. Index (a ': ctx) a
  S :: forall ctx a b. Index ctx a -> Index (b ': ctx) a

deriving instance Show (Index ctx a)

data Ctx :: (Type -> Type) -> [Type] -> Type where
  Nil :: Ctx f '[]
  Cons :: f a -> Ctx f as -> Ctx f (a ': as)

getCtx :: Index ctx a -> Ctx f ctx -> f a
getCtx Z (Cons a _) = a
getCtx (S ix) (Cons _ as) = getCtx ix as

data Quoted a = Quoted (Expr '[] a) a

instance Show (Quoted a) where
  showsPrec d (Quoted code _) =
    showParen (d > appPrec)
      $ showString "Quoted "
      . showsPrec (appPrec + 1) code
      . showString " <<function>>"
   where
    appPrec = 10

{-# NOINLINE quote #-}
quote :: (HasCallStack) => a -> Quoted a
quote a = noinline error "quote not removed by compiler plugin" a