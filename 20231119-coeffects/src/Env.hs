{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Env (
  -- * Coeffect
  Env,
  Var (..),
  Env.get,
  withEnvM,
  withEnv,

  -- * Effect

  -- ** Interface
  MonadEnviron (..),
  EnvironStateT (..),

  -- ** Semantics
  EnvironT (..),
  runEnvironT,

  -- * Internals
  HasVar,
  KnownSymbols,

  -- ** Escaping rules #escaping#

  -- *** Linux
  escapeKeyLinux,
  unescapeKeyLinux,
) where

import Control.Comonad (Comonad (..))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.Reader
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import qualified Control.Monad.State.Class
import Control.Monad.State.Strict (StateT, runStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Writer.Class (MonadWriter)
import qualified Control.Monad.Writer.Strict
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

data Env :: Type -> [Symbol] -> Type -> Type where
  Env :: [String] -> a -> Env s vars a

deriving instance Functor (Env s vars)

instance Comonad (Env s vars) where
  extract (Env _ a) = a
  duplicate (Env vars a) = Env vars (Env vars a)

data Var :: Symbol -> Type where
  Var :: (KnownSymbol s) => Var s

instance (KnownSymbol s, s ~ s') => IsLabel s' (Var s) where
  fromLabel = Var @s

class HasVar var vars where
  get :: Var var -> Env s vars a -> String

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
  forall vars m b.
  (KnownSymbols vars, MonadEnviron m) =>
  (forall s. Env s vars () -> m b) ->
  m b
withEnvM f = do
  let vars = symbolVals @vars
  (notFoundKeys, foundVars) <-
    partitionEithers
      <$> traverse
        (\var -> maybe (Left var) Right <$> getVar var)
        vars
  unless (null notFoundKeys)
    $ error
    $ "environment variable"
    <> (case notFoundKeys of [_] -> "s"; _ -> "")
    <> " not found: "
    <> intercalate ", " notFoundKeys
  f (Env foundVars ())

withEnv ::
  forall vars m b.
  (KnownSymbols vars, MonadEnviron m) =>
  (forall s. Env s vars () -> b) ->
  m b
withEnv f = withEnvM (pure . f)

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

instance (MonadEnviron m) => MonadEnviron (Control.Monad.Reader.ReaderT r m)
instance (MonadEnviron m, Monoid w) => MonadEnviron (Control.Monad.Writer.Strict.WriterT w m)
instance (MonadEnviron m) => MonadEnviron (Control.Monad.State.Strict.StateT s m)

-- | Every @'MonadEnviron' m@ instance has an associated @'MonadState' (Map String String) m@ instance.
newtype EnvironStateT m a = EnvironStateT {runEnvironStateT :: m a}
  deriving (Functor, Applicative, Monad, MonadEnviron, MonadIO, MonadReader r, MonadWriter w)

instance MonadTrans EnvironStateT where
  lift = EnvironStateT

instance (MonadEnviron m) => MonadState (Map String String) (EnvironStateT m) where
  get = EnvironStateT getEnviron
  put = EnvironStateT . setEnviron

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
    error $ "unexpected characters \"" <> s <> "\" in environment variable key \"" <> s <> "\""
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
    error $ "unexpected characters \"" <> s <> "\" in environment variable value \"" <> str <> "\""
  go (c : s) = c : go s
  go "" = ""

{- | Caveats:

* This instance is only valid under single-threaded environment variable manipulation.

  When multiple threads modify the environment, the interface becomes nondeterministic (you might
  not get back what you put in).

* Assumes that the preexisting environment variables are escaped according to the platform's
  [escaping rules](#g:escaping).
-}
instance MonadEnviron IO where
  getEnviron =
    Map.fromList
      . fmap (bimap unescapeKeyLinux unescapeValueLinux)
      <$> System.Environment.getEnvironment
  setEnviron newEnv = do
    env <- System.Environment.getEnvironment
    for_ env $ \(key, _) -> System.Environment.unsetEnv key
    for_ (Map.toList newEnv) $ \(key, value) ->
      System.Environment.setEnv (escapeKeyLinux key) (escapeValueLinux value) True
  getVar = fmap (fmap unescapeValueLinux) . System.Environment.lookupEnv . escapeKeyLinux
  setVar k v = System.Environment.setEnv (escapeKeyLinux k) (escapeValueLinux v) True

newtype EnvironT m a = EnvironT {unEnvironT :: StateT (Map String String) m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadReader r, MonadWriter w)

instance (MonadState s m) => MonadState s (EnvironT m) where
  get = EnvironT $ lift Control.Monad.State.Class.get
  put = EnvironT . lift . Control.Monad.State.Class.put

instance (Monad m) => MonadEnviron (EnvironT m) where
  getEnviron = EnvironT Control.Monad.State.Class.get
  setEnviron e = EnvironT $ Control.Monad.State.Class.put e
  getVar k = EnvironT (Control.Monad.State.Class.gets $ Map.lookup k)
  setVar k v = EnvironT (Control.Monad.State.Class.modify $ Map.insert k v)

runEnvironT :: (Monad m) => EnvironT m a -> Map String String -> m (Map String String, a)
runEnvironT ma s = do
  (a, s') <- runStateT (unEnvironT ma) s
  pure (s', a)