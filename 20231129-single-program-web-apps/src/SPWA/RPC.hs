{-# LANGUAGE RankNTypes #-}

module SPWA.RPC (RPC (..)) where

import qualified Data.ByteString.Lazy as Lazy
import SPWA.Session (SessionEnv)

newtype RPC = RPC (forall a. SessionEnv -> Lazy.ByteString -> (Lazy.ByteString -> IO a) -> IO a)