{-# LANGUAGE DerivingVia #-}

module SPWA.Template (Template (..)) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (IdentityT (..))
import Data.Monoid (Ap (..))
import Data.String (IsString (..))

newtype Template m a = Template {runTemplate :: m a}
  deriving (Functor, Applicative, Monad, MonadIO) via IdentityT m
  deriving (Semigroup, Monoid) via Ap m a

instance (Applicative m, IsString a) => IsString (Template m a) where
  fromString = Template . pure . fromString