{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Todo.V0 where

import BinaryCodec
import Data.ByteString (ByteString)
import Data.String (fromString)
import StateId (StateId)
import qualified Todo
import Todo.Task (TaskId)
import qualified Todo.Task as Todo (Task)
import Prelude hiding (map)

state :: Def (Codec Todo.State)
state =
  Def (fromString "State") (fromString "") $
    Body $
      Cast _ _ $
        Field (fromString "current") (set .@ stateId) $
          Field (fromString "tasks") (map .@ taskId .@ (task .@ (map .@ stateId))) $
            Field (fromString "taskMetadata") _ $
              Field (fromString "comments") _ $
                Field (fromString "commentMetadata") _ $
                  Field (fromString "history") _ $
                    Unit

stateHeader :: Def (Codec ByteString)
stateHeader =
  Def (fromString "StateHeader") (fromString "") $
    Body $
      _

stateId :: B ctx (Codec StateId)
stateId = Ref def
  where
    def =
      Def (fromString "StateId") (fromString "") $
        Body $
          _

taskId :: B ctx (Codec TaskId)
taskId = Ref def
  where
    def =
      Def (fromString "TaskId") (fromString "") $
        Body $
          _

task :: B ctx ((forall a. Codec a `Arr` Codec (f a)) `Arr` Codec (Todo.Task f))
task = Ref def
  where
    def =
      Def (fromString "Task") (fromString "") $
        Param (fromString "f") $
          Body $
            _
