{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module StateId where

import Data.Binary (Binary)
import MD5 (MD5 (..), md5ToBase32)

newtype StateId = StateId MD5
  deriving (Show, Eq, Ord)
  deriving newtype (Binary)

initialStateId :: StateId
initialStateId = StateId (MD5 0 0)

renderStateId :: StateId -> String
renderStateId (StateId s) = md5ToBase32 s
