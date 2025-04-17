{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Main where

import Data.Kind (Type)
import Data.ByteString.Builder (Builder)
import Data.String (fromString)
import Control.Monad.Trans.State.Strict (StateT, get, put, evalStateT)
import Data.Functor.Identity (Identity)
import Data.Functor.Const (Const (..))
import Barbies
import GHC.Generics (Generic)

data UnitB (f :: Type -> Type) = UnitB
  deriving (Generic, FunctorB, TraversableB)

data PairB a b f = PairB{ fstB :: f a, sndB :: f b }
  deriving (Generic, FunctorB, TraversableB)

class TraversableB b => HasColumns (b :: (Type -> Type) -> Type) where 
  columnsOf :: b (Const String)

instance HasColumns (PairB a b) where
  columnsOf = PairB (Const "fst") (Const "snd")

data Query :: Type -> Type -> Type where
  Id :: Query a a
  Compose :: Query b c -> Query a b -> Query a c
  
  Product :: HasColumns b => b (Query x) -> Query x (b Identity)
  Prj :: HasColumns b => (forall f. b f -> f a) -> Query (b Identity) a

class Monad m => MonadFresh m where
  fresh :: m Int

newtype FreshT m a = FreshT (StateT Int m a)
  deriving newtype (Functor, Applicative, Monad)

runFreshT :: Monad m => FreshT m a -> m a
runFreshT (FreshT ma) = evalStateT ma 0

instance Monad m => MonadFresh (FreshT m) where
  fresh = FreshT $ do
    n <- get
    put $ n + 1
    pure n

freshName :: MonadFresh m => Builder -> m Builder
freshName prefix = do
  n <- fresh
  pure $ prefix <> fromString (show n)

parens :: Builder -> Builder
parens x = "(" <> x <> ")"

sepBy :: Monoid m => m -> [m] -> m
sepBy _ [] = mempty
sepBy _ [x] = x
sepBy sep (x:xs) = x <> sep <> sepBy sep xs

selectFrom :: [Builder] -> Builder -> Builder
selectFrom cols table = "SELECT " <> sepBy ", " cols <> " FROM " <> table

toSQL :: MonadFresh m => Query a b -> Builder -> m Builder
toSQL q arg = do
  argName <- freshName "arg"
  (defs, cols, val) <- go (["it"], argName) q
  pure $ "WITH " <> sepBy ", " ((argName <> "(it)" <> " AS " <> parens arg) : defs) <> " " <> selectFrom cols val
  where
    go :: MonadFresh m => ([Builder], Builder) -> Query a b -> m ([Builder], [Builder], Builder)
    go (cols, val) Id = do
      pure (mempty, cols, val)
    go ctx (Compose f g) = do
      (defs, gCols, g') <- go ctx g
      (defs', fCols, f') <- go (gCols, g') f
      pure (defs <> defs', fCols, f')
    go ctx (Product @b parts)
      | null (columnsOf @b) = do
          resultName <- freshName "t"
          pure ([resultName <> "(it)" <> " AS " <> parens "SELECT null"], ["it"], resultName)
      | otherwise = do
          let columns = bfoldMap (pure . fromString . getConst) (columnsOf @b)
          results <- btraverse (fmap Const . go ctx) parts
          resultName <- freshName "t"
          pure
            ( bfoldMap (\(Const (defs, _, _)) -> defs) results <>
                [resultName <> "(" <> sepBy ", " columns <> ") AS (VALUES(" <> sepBy ", " (bfoldMap (\(Const (_, cols, q')) -> [parens $ selectFrom cols q']) results) <> "))"]
            , columns
            , resultName
            )
    go (_cols, table) (Prj @b f) = do
      pure (mempty, [fromString . getConst $ f (columnsOf @b)], table)

main :: IO ()
main = putStrLn "Hello, Haskell!"
