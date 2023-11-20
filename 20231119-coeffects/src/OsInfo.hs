{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module OsInfo (OsInfo, osInfo, osName) where

import Control.Comonad (Comonad (..))
import Control.Comonad.Env (Env, asks, env)
import qualified System.Info

newtype OsInfo a = OsInfo (Env OsInfo' a)
  deriving (Functor)

instance Comonad OsInfo where
  extract (OsInfo wa) = extract wa
  duplicate (OsInfo wa) = OsInfo (OsInfo <$> duplicate wa)

data OsInfo' = OsInfo' {osName' :: String}

osInfo :: OsInfo ()
osInfo = OsInfo (env OsInfo'{osName' = System.Info.os} ())

osName :: OsInfo a -> String
osName (OsInfo wa) = asks osName' wa
