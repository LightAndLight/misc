{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module SystemInfo (SystemInfo, withSystemInfo, os, arch) where

import Control.Comonad (Comonad (..))
import Control.Comonad.Env (Env, asks, env)
import qualified System.Info

newtype SystemInfo s a = SystemInfo (Env SystemInfo' a)
  deriving (Functor)

instance Comonad (SystemInfo s) where
  extract (SystemInfo wa) = extract wa
  duplicate (SystemInfo wa) = SystemInfo (SystemInfo <$> duplicate wa)

data SystemInfo' = SystemInfo' {os' :: String, arch' :: String}

systemInfo :: SystemInfo s ()
systemInfo =
  SystemInfo
    ( env
        SystemInfo'
          { os' = System.Info.os
          , arch' = System.Info.arch
          }
        ()
    )

os :: SystemInfo s a -> String
os (SystemInfo wa) = asks os' wa

arch :: SystemInfo s a -> String
arch (SystemInfo wa) = asks arch' wa

withSystemInfo :: (forall s. SystemInfo s () -> a) -> a
withSystemInfo f = f systemInfo
