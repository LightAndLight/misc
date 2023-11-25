{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Env (
  Env,
  Var (..),
  Env.get,
  withEnvM,
  withEnv,
  loadEnv,

  -- * Effect
  MonadEnviron (..),
  EnvironT (..),
  runEnvironT,

  -- * Internals
  HasVar,
  KnownSymbols,
) where

import Control.Comonad (Comonad (..))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (StateT, get, gets, modify, put, runStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Bifunctor (bimap)
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Stack (HasCallStack, withFrozenCallStack)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified System.Environment (lookupEnv)
import qualified System.Environment.Blank as System.Environment

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
  get key (Env (_ : values) a) = Env.get @var @vars key (Env values a)
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
        (\var -> maybe (Left var) Right <$> System.Environment.lookupEnv var)
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
        (\var -> maybe (Left var) Right <$> System.Environment.lookupEnv var)
        vars
  unless (null notFoundKeys)
    $ error
    $ "environment variable"
    <> (case notFoundKeys of [_] -> "s"; _ -> "")
    <> " not found: "
    <> intercalate ", " notFoundKeys
  pure (Env foundVars ())

{- | The semantics of environment variable manipulation is given by 'EnvironT'.

@[[ m ]] = λa. Map String String -> (Map String String, a)@
-}
class (Monad m) => MonadEnviron m where
  -- | @[[ getEnviron ]] = \\e -> (e, e)@
  getEnviron :: (HasCallStack) => m (Map String String)
  default getEnviron :: (m ~ t n, MonadTrans t, MonadEnviron n) => m (Map String String)
  getEnviron = lift getEnviron

  -- | @[[ setEnviron e' ]] = \\_ -> (e', ())@
  setEnviron :: (HasCallStack) => Map String String -> m ()
  default setEnviron :: (m ~ t n, MonadTrans t, MonadEnviron n) => Map String String -> m ()
  setEnviron = lift . setEnviron

  -- | @[[ getVar k ]] = \\e -> (e, lookup k e)@
  getVar :: (HasCallStack) => String -> m (Maybe String)
  default getVar :: (m ~ t n, MonadTrans t, MonadEnviron n) => String -> m (Maybe String)
  getVar = lift . getVar

  -- | @[[ setVar k v ]] = \\e -> (insert k v e, ())@
  setVar :: (HasCallStack) => String -> String -> m ()
  default setVar :: (m ~ t n, MonadTrans t, MonadEnviron n) => String -> String -> m ()
  setVar k = lift . setVar k

escapeKeyLinux :: String -> String
escapeKeyLinux "" = "\\empty"
escapeKeyLinux s = s >>= escape
 where
  escape '\\' = "\\\\"
  escape '\0' = "\\x0"
  escape '=' = "\\equals"
  escape c = [c]

unescapeKeyLinux :: (HasCallStack) => String -> String
unescapeKeyLinux str = withFrozenCallStack (go str)
 where
  go "\\empty" = ""
  go ('\\' : '\\' : s) = '\\' : go s
  go ('\\' : 'x' : '0' : s) = '\0' : go s
  go ('\\' : 'e' : 'q' : 'u' : 'a' : 'l' : 's' : s) = '=' : go s
  go s@('\\' : _) =
    error $ "unexpected characters in environment variable key: " <> show s
  go (c : s) = c : go s
  go "" = ""

escapeValueLinux :: String -> String
escapeValueLinux "" = ""
escapeValueLinux s = s >>= escape
 where
  escape '\\' = "\\\\"
  escape '\0' = "\\x0"
  escape c = [c]

unescapeValueLinux :: (HasCallStack) => String -> String
unescapeValueLinux str = withFrozenCallStack (go str)
 where
  go ('\\' : '\\' : s) = '\\' : go s
  go ('\\' : 'x' : '0' : s) = '\0' : go s
  go s@('\\' : _) =
    error $ "unexpected characters in environment variable value: " <> show s
  go (c : s) = c : go s
  go "" = ""

instance MonadEnviron IO where
  getEnviron =
    fmap (Map.fromList . fmap (bimap unescapeKeyLinux unescapeValueLinux))
      $ System.Environment.getEnvironment
  setEnviron newEnv = do
    env <- System.Environment.getEnvironment
    for_ env $ \(key, _) -> System.Environment.unsetEnv key
    for_ (Map.toList newEnv) $ \(key, value) ->
      System.Environment.setEnv (escapeKeyLinux key) (escapeValueLinux value) True
  getVar = fmap (fmap unescapeValueLinux) . System.Environment.lookupEnv . escapeKeyLinux
  setVar k v = System.Environment.setEnv (escapeKeyLinux k) (escapeValueLinux v) True

newtype EnvironT m a = EnvironT {unEnvironT :: StateT (Map String String) m a}
  deriving (Functor, Applicative, Monad, MonadTrans)

instance (Monad m) => MonadEnviron (EnvironT m) where
  getEnviron = EnvironT Control.Monad.State.Strict.get
  setEnviron e = EnvironT $ put e
  getVar k = EnvironT (gets $ Map.lookup k)
  setVar k v = EnvironT (modify $ Map.insert k v)

runEnvironT :: (Monad m) => EnvironT m a -> Map String String -> m (Map String String, a)
runEnvironT ma s = do
  (a, s') <- runStateT (unEnvironT ma) s
  pure (s', a)