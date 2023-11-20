{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Env (
  Env,
  get,
  withEnvM,
  withEnv,
  loadEnv,

  -- * Internals
  HasVar,
) where

import Control.Comonad (Comonad (..))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either (partitionEithers)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Proxy (Proxy (..))
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import System.Environment (lookupEnv)

data Env :: [Symbol] -> Type -> Type where
  Env :: [String] -> a -> Env vars a

deriving instance Functor (Env vars)

instance Comonad (Env vars) where
  extract (Env _ a) = a
  duplicate (Env vars a) = Env vars (Env vars a)

data Var :: Symbol -> Type where
  Var :: (KnownSymbol s) => Var s

instance (KnownSymbol s, s ~ s') => IsLabel s' (Var s) where
  fromLabel = Var @s

class HasVar var vars where
  get :: Var var -> Env vars a -> String

instance {-# OVERLAPPING #-} HasVar var (var ': vars) where
  get _ (Env (value : _) _) = value
  get _ (Env _ _) = undefined

instance (HasVar var vars) => HasVar var (var' ': vars) where
  get key (Env (_ : values) a) = get @var @vars key (Env values a)
  get _ (Env _ _) = undefined

class KnownSymbols (syms :: [Symbol]) where
  symbolVals :: [String]

instance KnownSymbols '[] where
  symbolVals = []

instance (KnownSymbol a, KnownSymbols as) => KnownSymbols (a ': as) where
  symbolVals = symbolVal (Proxy :: Proxy a) : symbolVals @as

withEnvM ::
  forall vars m a b.
  (KnownSymbols vars, MonadIO m) =>
  (Env vars a -> m b) ->
  a ->
  m b
withEnvM f a = do
  let vars = symbolVals @vars
  (notFoundKeys, foundVars) <-
    liftIO
      $ partitionEithers
      <$> traverse
        (\var -> maybe (Left var) Right <$> lookupEnv var)
        vars
  unless (null notFoundKeys)
    $ error
    $ "environment variable"
    <> (case notFoundKeys of [_] -> "s"; _ -> "")
    <> " not found: "
    <> intercalate ", " notFoundKeys
  f (Env foundVars a)

withEnv ::
  forall vars m a b.
  (KnownSymbols vars, MonadIO m) =>
  (Env vars a -> b) ->
  a ->
  m b
withEnv f = withEnvM (pure . f)

loadEnv :: forall vars m. (KnownSymbols vars, MonadIO m) => m (Env vars ())
loadEnv = do
  let vars = symbolVals @vars
  (notFoundKeys, foundVars) <-
    liftIO
      $ partitionEithers
      <$> traverse
        (\var -> maybe (Left var) Right <$> lookupEnv var)
        vars
  unless (null notFoundKeys)
    $ error
    $ "environment variable"
    <> (case notFoundKeys of [_] -> "s"; _ -> "")
    <> " not found: "
    <> intercalate ", " notFoundKeys
  pure (Env foundVars ())
