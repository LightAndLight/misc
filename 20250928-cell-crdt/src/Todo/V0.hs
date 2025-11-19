{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}

module Todo.V0 where

import BinaryCodec
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.String (fromString)
import Data.Void (Void, absurd)
import GID (GID)
import Generics.Eot (Eot, HasEot, fromEot, toEot)
import MD5 (MD5)
import StateId (StateId (..))
import qualified Todo
import Todo.Comment (CommentId)
import qualified Todo.Comment as Todo (Comment, CommentMetadata)
import Todo.Task (TaskId (..))
import qualified Todo.Task as Todo (Task, TaskMetadata)
import Prelude hiding (map)

record ::
  (HasEot record, Eot record ~ Either fields Void) =>
  B ctx (Codec fields) ->
  B ctx (Codec record)
record = Cast (fromEot . Left) (Just . Prelude.either id absurd . toEot)

state :: Def (Codec Todo.State)
state =
  Def (fromString "State") (fromString "") $
    Body
      ( record
          . Field (fromString "current") (set .@ stateId)
          . Field (fromString "tasks") (map .@ taskId .@ task)
          . Field (fromString "taskMetadata") (map .@ taskId .@ taskMetadata)
          . Field (fromString "comments") (map .@ commentId .@ comment)
          . Field (fromString "commentMetadata") (map .@ commentId .@ commentMetadata)
          . Field (fromString "history") (map .@ stateId .@ commit)
          $ Unit
      )

stateHeader :: Def (Codec ())
stateHeader =
  Def (fromString "StateHeader") (fromString "")
    $ Body
    $ Cast (\((), ((), ((), ()))) -> ()) (\() -> Just ((), ((), ((), ()))))
      . Field (fromString "fileType") (literalUtf8 $ fromString "tsk;")
      . Field (fromString "version") (literalUtf8 $ fromString "version=0;")
      . Field (fromString "endOfHeader") (literalUtf8 $ fromString "\n")
    $ Unit

md5 :: B ctx (Codec MD5)
md5 = Ref def
  where
    def =
      Def (fromString "MD5") (fromString "")
        $ Body
        $ record
          . Field (fromString "0") word64
          . Field (fromString "1") word64
        $ Unit

stateId :: B ctx (Codec StateId)
stateId = Ref def
  where
    def =
      Def (fromString "StateId") (fromString "") $
        Body $
          newtype_ md5

gid :: B ctx (Codec GID)
gid = Ref def
  where
    def =
      Def (fromString "GID") (fromString "")
        $ Body
        $ record
          . Field (fromString "0") word16
          . Field (fromString "1") word16
          . Field (fromString "2") word16
          . Field (fromString "3") word16
          . Field (fromString "4") word16
        $ Unit

taskId :: B ctx (Codec TaskId)
taskId = Ref def
  where
    def =
      Def (fromString "TaskId") (fromString "") $
        Body $
          newtype_ gid

-- I'd love to have this be ``B ctx ((forall a. Codec a `Arr` Codec (f a)) `Arr` Codec (Todo.Task f))`` but I couldn't get it working.
task :: B ctx (Codec (Todo.Task (Map StateId)))
task = Ref def
  where
    def =
      Def (fromString "Task") (fromString "")
        $ Body
        $ record
          . Field (fromString "status") (map .@ stateId .@ utf8)
          . Field (fromString "labels") (map .@ stateId .@ (set .@ utf8))
          . Field (fromString "title") (map .@ stateId .@ utf8)
          . Field (fromString "description") (map .@ stateId .@ utf8)
        $ Unit

taskMetadata :: B ctx (Codec Todo.TaskMetadata)
taskMetadata = Ref def
  where
    def =
      Def (fromString "TaskMetadata") (fromString "")
        $ Body
        $ record
          . Field (fromString "createdAt") utcTime
        $ Unit

commentId :: B ctx (Codec CommentId)
commentId = Ref def
  where
    def =
      Def (fromString "CommentId") (fromString "") $
        Body $
          _

comment :: B ctx (Codec (Todo.Comment (Map StateId)))
comment = Ref def
  where
    def =
      Def (fromString "Comment") (fromString "") $
        Body $
          _

commentMetadata :: B ctx (Codec Todo.CommentMetadata)
commentMetadata = Ref def
  where
    def =
      Def (fromString "CommentMetadata") (fromString "") $
        Body $
          _

commit :: B ctx (Codec Todo.Commit)
commit = Ref def
  where
    def =
      Def (fromString "Commit") (fromString "") $
        Body $
          _
