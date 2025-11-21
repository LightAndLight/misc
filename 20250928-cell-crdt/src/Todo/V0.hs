{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Todo.V0 where

import BinaryCodec
import Data.Functor.Identity (Identity (..))
import Data.Map.Strict (Map)
import Data.String (fromString)
import GID (GID)
import MD5 (MD5)
import StateId (StateId (..))
import qualified Todo
import Todo.Comment (CommentId (..))
import qualified Todo.Comment as Todo (Comment, CommentMetadata, ReplyId (..))
import Todo.Task (TaskId (..))
import qualified Todo.Task as Todo (Task, TaskMetadata, Update (..))
import Prelude hiding (map)

state :: Def (Codec Todo.State)
state =
  Def (fromString "State") (fromString "")
    $ Body
    $ iso snd ((,) ())
    $ record
      . Field (fromString "header") stateHeader
      . Field
        (fromString "data")
        ( record
            . Field (fromString "current") (set .@ stateId)
            . Field (fromString "tasks") (map .@ taskId .@ task)
            . Field (fromString "taskMetadata") (map .@ taskId .@ taskMetadata)
            . Field (fromString "comments") (map .@ commentId .@ comment)
            . Field (fromString "commentMetadata") (map .@ commentId .@ commentMetadata)
            . Field (fromString "history") (map .@ stateId .@ commit)
            $ Unit
        )
    $ Unit

stateHeader :: B ctx (Codec ())
stateHeader = Ref def
  where
    def =
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

taskType ::
  (forall ctx' a. B ctx' (Codec a) -> B ctx' (Codec (f a))) ->
  B ctx (Codec (Todo.Task f))
taskType f =
  record
    . Field (fromString "status") (f utf8)
    . Field (fromString "labels") (f (set .@ utf8))
    . Field (fromString "title") (f utf8)
    . Field (fromString "description") (f utf8)
    $ Unit

-- I'd love to have this be ``B ctx ((forall a. Codec a `Arr` Codec (f a)) `Arr` Codec (Todo.Task f))`` but I couldn't get it working.
task :: B ctx (Codec (Todo.Task (Map StateId)))
task = Ref def
  where
    def =
      Def (fromString "Task") (fromString "") $
        Body $
          taskType ((map .@ stateId) .@)

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
          newtype_ gid

commentType ::
  (forall ctx' a. B ctx' (Codec a) -> B ctx' (Codec (f a))) ->
  B ctx (Codec (Todo.Comment f))
commentType f =
  record
    . Field (fromString "description") (f utf8)
    $ Unit

comment :: B ctx (Codec (Todo.Comment (Map StateId)))
comment = Ref def
  where
    def =
      Def (fromString "Comment") (fromString "") $
        Body $
          commentType ((map .@ stateId) .@)

replyId :: B ctx (Codec Todo.ReplyId)
replyId = Ref def
  where
    def =
      Def (fromString "ReplyId") (fromString "")
        $ Body
        $ iso
          snd
          ( \input ->
              case input of
                Todo.ReplyTask{} -> (0, input)
                Todo.ReplyComment{} -> (1, input)
          )
        $ record
          . Field (fromString "tag") word64
          . Field
            (fromString "value")
            ( Choice
                (Var Z)
                [ (0, Cast Todo.ReplyTask (\case Todo.ReplyTask x -> Just x; _ -> Nothing) taskId)
                , (1, Cast Todo.ReplyComment (\case Todo.ReplyComment x -> Just x; _ -> Nothing) commentId)
                ]
            )
        $ Unit

commentMetadata :: B ctx (Codec Todo.CommentMetadata)
commentMetadata = Ref def
  where
    def =
      Def (fromString "CommentMetadata") (fromString "")
        $ Body
        $ record
          . Field (fromString "createdAt") utcTime
          . Field (fromString "replyTo") replyId
        $ Unit

commit :: B ctx (Codec Todo.Commit)
commit = Ref def
  where
    def =
      Def (fromString "Commit") (fromString "")
        $ Body
        $ record
          . Field (fromString "parents") (set .@ stateId)
          . Field (fromString "change") change
        $ Unit

update :: B ctx (Codec a `Arr` Codec (Todo.Update a))
update = Ref def
  where
    def =
      Def (fromString "Update") (fromString "")
        $ Param (fromString "a")
        $ Body
        $ iso
          snd
          ( \input ->
              case input of
                Todo.None -> (0, input)
                Todo.Set{} -> (1, input)
                Todo.Pick{} -> (2, input)
          )
        $ record
          . Field (fromString "tag") word64
          . Field
            (fromString "value")
            ( Choice
                (Var Z)
                [
                  ( 0
                  , Cast
                      (\() -> Todo.None)
                      (\case Todo.None -> Just (); _ -> Nothing)
                      Unit
                  )
                ,
                  ( 1
                  , Cast
                      Todo.Set
                      (\case Todo.Set x -> Just x; _ -> Nothing)
                      (Var (S Z))
                  )
                ,
                  ( 2
                  , Cast
                      Todo.Pick
                      (\case Todo.Pick x -> Just x; _ -> Nothing)
                      stateId
                  )
                ]
            )
        $ Unit

change :: B ctx (Codec Todo.Change)
change = Ref def
  where
    def =
      Def (fromString "Change") (fromString "")
        $ Body
        $ iso
          snd
          ( \input ->
              case input of
                Todo.NewTask{} -> (0, input)
                Todo.UpdateTask{} -> (1, input)
                Todo.NewComment{} -> (2, input)
                Todo.UpdateComment{} -> (3, input)
          )
        $ record
          . Field (fromString "tag") word64
          . Field
            (fromString "value")
            ( Choice
                (Var Z)
                [
                  ( 0
                  , Cast
                      (\task -> Todo.NewTask{Todo.task})
                      (\case Todo.NewTask x -> Just x; _ -> Nothing)
                      (taskType @Identity newtype_)
                  )
                ,
                  ( 1
                  , Cast
                      (\(taskId, updateTask) -> Todo.UpdateTask{Todo.taskId, Todo.updateTask})
                      (\case Todo.UpdateTask x y -> Just (x, y); _ -> Nothing)
                      ( record
                          . Field (fromString "taskId") taskId
                          . Field (fromString "updateTask") (taskType (update .@))
                          $ Unit
                      )
                  )
                ,
                  ( 2
                  , Cast
                      (\(replyTo, comment) -> Todo.NewComment{Todo.replyTo, Todo.comment})
                      (\case Todo.NewComment x y -> Just (x, y); _ -> Nothing)
                      ( record
                          . Field (fromString "replyTo") replyId
                          . Field (fromString "comment") (commentType @Identity newtype_)
                          $ Unit
                      )
                  )
                ,
                  ( 3
                  , Cast
                      (\(commentId, updateComment) -> Todo.UpdateComment{Todo.commentId, Todo.updateComment})
                      (\case Todo.UpdateComment x y -> Just (x, y); _ -> Nothing)
                      ( record
                          . Field (fromString "commentId") commentId
                          . Field (fromString "updateComment") (commentType @Todo.Update (update .@))
                          $ Unit
                      )
                  )
                ]
            )
        $ Unit
