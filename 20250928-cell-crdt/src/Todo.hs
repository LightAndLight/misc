{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Todo where

import Barbies
import Barbies.Constraints (Dict (..))
import Control.Exception (Exception)
import Control.Monad (guard, replicateM)
import Data.Binary (Binary, Word64, Word8)
import qualified Data.Binary as Binary
import Data.Binary.Put (runPut)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Fixed (Milli, Pico)
import Data.Foldable (foldl', traverse_)
import Data.Functor.Identity (Identity (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Monoid (Any (..), getAny)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime, getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import GHC.Generics (Generic)
import Getter (Getter (..))
import MD5 (hashMD5)
import StateId (StateId (..))
import Todo.Comment
  ( Comment
  , CommentId
  , CommentMetadata (CommentMetadata)
  , ReplyId (..)
  , commentGetters
  , newCommentId
  )
import qualified Todo.Comment as Comment
import Todo.Task (Task, TaskId, TaskMetadata (..), Update (..), newTaskId, taskGetters)
import qualified Todo.Task as Task

data Change
  = NewTask {task :: !(Task Identity)}
  | UpdateTask {taskId :: !TaskId, updateTask :: !(Task Update)}
  | NewComment {replyTo :: !ReplyId, comment :: !(Comment Identity)}
  | UpdateComment {commentId :: !CommentId, updateComment :: !(Comment Update)}
  deriving (Show, Eq, Generic)

mkStateId :: Set StateId -> Change -> StateId
mkStateId parents change = StateId . hashMD5 . runPut $ putSet parents <> putChange change

data Commit = Commit {parents :: !(Set StateId), change :: !Change}
  deriving (Show, Generic)

data State
  = State
  { current :: !(Set StateId)
  , tasks :: !(Map TaskId (Task (Map StateId)))
  , taskMetadata :: !(Map TaskId TaskMetadata)
  , comments :: !(Map CommentId (Comment (Map StateId)))
  , commentMetadata :: !(Map CommentId CommentMetadata)
  , history :: !(Map StateId Commit)
  }
  deriving (Show, Generic)

stateNew :: State
stateNew =
  State
    { current = Set.empty
    , tasks = Map.empty
    , taskMetadata = Map.empty
    , comments = Map.empty
    , commentMetadata = Map.empty
    , history = Map.empty
    }

stateChange :: Change -> State -> IO State
stateChange change@NewTask{task} state = do
  let stateId = mkStateId (current state) change
  now <- getCurrentTime

  taskId <- newTaskId
  pure
    state
      { current = Set.singleton stateId
      , tasks = Map.insert taskId (bmap (Map.singleton stateId . runIdentity) task) (tasks state)
      , taskMetadata = Map.insert taskId TaskMetadata{Task.createdAt = now} (taskMetadata state)
      , history = Map.insert stateId (Commit (current state) change) (history state)
      }
stateChange change@UpdateTask{taskId, updateTask} state = do
  let stateId = mkStateId (current state) change

  case Map.lookup taskId (tasks state) of
    Nothing -> error $ show taskId ++ " not found"
    Just task -> do
      pure
        state
          { current = Set.singleton stateId
          , tasks =
              Map.insert
                taskId
                ( bzipWith
                    ( \current update ->
                        case update of
                          None -> current
                          Set a -> Map.singleton stateId a
                          Pick stateId' ->
                            Map.singleton stateId $
                              fromMaybe
                                (error $ show stateId' ++ " missing from " ++ show (Map.keys current))
                                (Map.lookup stateId' current)
                    )
                    task
                    updateTask
                )
                (tasks state)
          , history = Map.insert stateId (Commit (current state) change) (history state)
          }
stateChange change@NewComment{replyTo, comment} state = do
  let stateId = mkStateId (current state) change
  now <- getCurrentTime

  commentId <- newCommentId
  pure
    state
      { current = Set.singleton stateId
      , comments =
          Map.insert commentId (bmap (Map.singleton stateId . runIdentity) comment) (comments state)
      , commentMetadata =
          Map.insert
            commentId
            CommentMetadata{Comment.createdAt = now, Comment.replyTo}
            (commentMetadata state)
      , history = Map.insert stateId (Commit (current state) change) (history state)
      }
stateChange change@UpdateComment{commentId, updateComment} state = do
  let stateId = mkStateId (current state) change

  case Map.lookup commentId (comments state) of
    Nothing -> error $ show commentId ++ " not found"
    Just comment -> do
      pure
        state
          { current = Set.singleton stateId
          , comments =
              Map.insert
                commentId
                ( bzipWith
                    ( \current update ->
                        case update of
                          None -> current
                          Set a -> Map.singleton stateId a
                          Pick stateId' ->
                            Map.singleton stateId $
                              fromMaybe
                                (error $ show stateId' ++ " missing from " ++ show (Map.keys current))
                                (Map.lookup stateId' current)
                    )
                    comment
                    updateComment
                )
                (comments state)
          , history = Map.insert stateId (Commit (current state) change) (history state)
          }

fastForwardTask ::
  Map (Set StateId) (Map StateId Change) ->
  TaskId ->
  Task (Map StateId) ->
  Task (Map StateId)
fastForwardTask edges taskId =
  bzipWith
    ( \(Getter getter) ->
        fastForward
          ( \case
              NewTask{task = task'} ->
                Just (const $ runIdentity (getter task'))
              UpdateTask{taskId = taskId', updateTask} -> do
                guard $ taskId == taskId'
                case getter updateTask of
                  None ->
                    Nothing
                  Set a ->
                    Just (const a)
                  Pick stateId ->
                    Just
                      ( \current ->
                          fromMaybe (error $ show stateId ++ " missing from" ++ show (Map.keys current)) $
                            Map.lookup stateId current
                      )
              NewComment{} ->
                Nothing
              UpdateComment{} ->
                Nothing
          )
          edges
    )
    taskGetters

fastForwardComment ::
  Map (Set StateId) (Map StateId Change) ->
  CommentId ->
  Comment (Map StateId) ->
  Comment (Map StateId)
fastForwardComment edges commentId =
  bzipWith
    ( \(Getter getter) ->
        fastForward
          ( \case
              NewComment{comment = comment'} ->
                Just (const $ runIdentity (getter comment'))
              UpdateComment{commentId = commentId', updateComment} -> do
                guard $ commentId == commentId'
                case getter updateComment of
                  None ->
                    Nothing
                  Set a ->
                    Just (const a)
                  Pick stateId ->
                    Just
                      ( \current ->
                          fromMaybe (error $ show stateId ++ " missing from" ++ show (Map.keys current)) $
                            Map.lookup stateId current
                      )
              NewTask{} ->
                Nothing
              UpdateTask{} ->
                Nothing
          )
          edges
    )
    commentGetters

fastForward ::
  (Change -> Maybe (Map StateId a -> a)) ->
  Map (Set StateId) (Map StateId Change) ->
  Map StateId a ->
  Map StateId a
fastForward fromChange edges input =
  {- The use of `powerSet` to check all subsets of the current state means this
  function has worst-case time complexity `O(2^v log v) * O(log e)`, where
  `v` is the size of the vertex (i.e. number of conflicts for the datum) and `e`
  is the total number of edges.
  -}
  let
    output =
      foldMap
        ( \source ->
            case Map.lookup source edges of
              Nothing -> Map.empty
              Just changes -> ($ input) <$> Map.mapMaybe fromChange changes
        )
        (Set.toList $ Set.powerSet $ Map.keysSet input)
  in
    if Map.null output
      then input
      else fastForward fromChange edges output

fastForward' ::
  Map (Set StateId) (Map StateId Change) ->
  Set StateId ->
  Set StateId
fastForward' edges input =
  {- The use of `powerSet` to check all subsets of the current state means this
  function has worst-case time complexity `O(2^v log v) * O(log e)`, where
  `v` is the size of the vertex (i.e. number of conflicts for the datum) and `e`
  is the total number of edges.
  -}
  let
    output =
      foldMap
        (\source -> maybe Set.empty Map.keysSet $ Map.lookup source edges)
        (Set.toList $ Set.powerSet input)
  in
    if Set.null output
      then input
      else fastForward' edges output

stateMerge :: State -> State -> State
stateMerge s1 s2 =
  let
    newHistory = history s1 <> history s2
    currentStateIds = current s1 <> current s2

    mLca = lowestCommonAncestor newHistory currentStateIds

    (newCurrent, newTasks, newComments) =
      case mLca of
        Nothing ->
          ( current s1 <> current s2
          , Map.unionWith (bzipWith (<>)) (tasks s1) (tasks s2)
          , Map.unionWith (bzipWith (<>)) (comments s1) (comments s2)
          )
        Just lca ->
          let edges = edgesToLca newHistory lca currentStateIds
          in ( fastForward' edges (current s1) <> fastForward' edges (current s2)
             , Map.unionWith
                (bzipWith (<>))
                (Map.mapWithKey (fastForwardTask edges) (tasks s1))
                (Map.mapWithKey (fastForwardTask edges) (tasks s2))
             , Map.unionWith
                (bzipWith (<>))
                (Map.mapWithKey (fastForwardComment edges) (comments s1))
                (Map.mapWithKey (fastForwardComment edges) (comments s2))
             )
  in
    State
      { current = newCurrent
      , tasks = newTasks
      , taskMetadata = taskMetadata s1 <> taskMetadata s2
      , comments = newComments
      , commentMetadata = commentMetadata s1 <> commentMetadata s2
      , history = newHistory
      }

edgesToLca ::
  Map StateId Commit ->
  StateId ->
  Set StateId ->
  Map (Set StateId) (Map StateId Change)
edgesToLca history lca = go Map.empty
  where
    go ::
      Map (Set StateId) (Map StateId Change) ->
      Set StateId ->
      Map (Set StateId) (Map StateId Change)
    go acc stateIds
      | Set.null stateIds = acc
      | otherwise =
          let
            (acc', stateIds') =
              foldMap
                ( \stateId ->
                    case Map.lookup stateId history of
                      Just (Commit parents change)
                        | stateId /= lca ->
                            (Map.singleton parents (Map.singleton stateId change), parents)
                      _ ->
                        mempty
                )
                stateIds
          in
            go (Map.unionWith (<>) acc acc') stateIds'

enroute :: Map StateId Commit -> StateId -> Set StateId
enroute history stateId =
  case Map.lookup stateId history of
    Nothing -> Set.empty
    Just (Commit parents _) ->
      case Set.toList parents of
        [] ->
          Set.empty
        x : xs ->
          Set.insert stateId $
            foldl'
              (\acc parentStateId -> Set.intersection acc (enroute history parentStateId))
              (enroute history x)
              xs

lowestCommonAncestor :: Map StateId Commit -> Set StateId -> Maybe StateId
lowestCommonAncestor history = loop . Set.toList
  where
    loop stateIds =
      case stateIds of
        [] -> Nothing
        x : xs ->
          case xs of
            [] -> Just x
            y : ys -> do
              y' <- go (enroute history x) y
              loop $ y' : ys

    go enrouteSet b
      | b `Set.member` enrouteSet = Just b
      | otherwise =
          case Map.lookup b history of
            Nothing -> Nothing
            Just (Commit parents _) ->
              case Set.toList parents of
                [b'] -> go enrouteSet b'
                _ -> do
                  b' <- lowestCommonAncestor history parents
                  go enrouteSet b'

stateConflicts :: State -> Map TaskId (Task (Map StateId))
stateConflicts state = Map.filter (getAny . bfoldMap (Any . (1 <) . Map.size)) (tasks state)

data History
  = Nil
  | Sequential History (Change, StateId)
  | Concurrent History [History]
  deriving (Show, Eq)

stateHistory :: State -> History
stateHistory versioned = go Nothing (current versioned)
  where
    go :: Maybe StateId -> Set StateId -> History
    go mTop stateIds
      | Set.null stateIds = Nil
      | Just top <- mTop, top `Set.member` stateIds = Nil
      | Set.size stateIds == 1
      , let stateId = Set.findMax stateIds =
          case Map.lookup stateId (history versioned) of
            Nothing ->
              Nil
            Just (Commit previous change) ->
              Sequential (go mTop previous) (change, stateId)
      | otherwise =
          let
            mAncestor = lowestCommonAncestor (history versioned) stateIds
          in
            Concurrent
              (go mTop $ maybe Set.empty Set.singleton mAncestor)
              (go mAncestor . Set.singleton <$> Set.toList stateIds)

stateBinaryPut :: State -> Binary.Put
stateBinaryPut (State current tasks taskMetadata comments commentMetadata history) = do
  traverse_ Binary.put . ByteString.unpack $ ByteString.Char8.pack "tsk;version=0;\n"
  putCurrent current
  putTasks tasks
  putTaskMetadata taskMetadata
  putComments comments
  putCommentMetadata commentMetadata
  putHistory history

stateBinaryGet :: Binary.Get State
stateBinaryGet = do
  current <- getCurrent
  tasks <- getTasks
  taskMetadata <- getTaskMetadata
  comments <- getComments
  commentMetadata <- getCommentMetadata
  history <- getHistory
  pure State{current, tasks, taskMetadata, comments, commentMetadata, history}

putSet :: Binary a => Set a -> Binary.Put
putSet = Binary.put . Set.toAscList

getSet :: (Binary a, Eq a) => Binary.Get (Set a)
getSet = Set.fromAscList <$> Binary.get

putCurrent :: Set StateId -> Binary.Put
putCurrent = putSet

getCurrent :: Binary.Get (Set StateId)
getCurrent = getSet

putList :: (a -> Binary.Put) -> [a] -> Binary.Put
putList put xs = do
  Binary.put @Word64 . fromIntegral $ length xs
  traverse_ put xs

getList :: Binary.Get a -> Binary.Get [a]
getList get = do
  n <- Binary.get @Word64
  replicateM (fromIntegral n) get

putMap :: Binary k => (v -> Binary.Put) -> Map k v -> Binary.Put
putMap put = putList (\(k, v) -> Binary.put k <> put v) . Map.toAscList

getMap :: (Binary k, Eq k) => Binary.Get v -> Binary.Get (Map k v)
getMap get = Map.fromAscList <$> getList ((,) <$> Binary.get <*> get)

putTasks :: Map TaskId (Task (Map StateId)) -> Binary.Put
putTasks = putMap (bfoldMapC @Binary (putMap Binary.put))

getTasks :: Binary.Get (Map TaskId (Task (Map StateId)))
getTasks = getMap (btraverse (\Dict -> getMap Binary.get) (bdicts @Binary))

putTaskMetadata :: Map TaskId TaskMetadata -> Binary.Put
putTaskMetadata = putMap putMetadata
  where
    putMetadata :: TaskMetadata -> Binary.Put
    putMetadata (TaskMetadata createdAt) = do
      putUTCTime createdAt

getTaskMetadata :: Binary.Get (Map TaskId TaskMetadata)
getTaskMetadata = getMap getMetadata
  where
    getMetadata :: Binary.Get TaskMetadata
    getMetadata = do
      createdAt <- getUTCTime
      pure TaskMetadata{Task.createdAt}

putComments :: Map CommentId (Comment (Map StateId)) -> Binary.Put
putComments = putMap (bfoldMapC @Binary (putMap Binary.put))

getComments :: Binary.Get (Map CommentId (Comment (Map StateId)))
getComments = getMap (btraverse (\Dict -> getMap Binary.get) (bdicts @Binary))

putCommentMetadata :: Map CommentId CommentMetadata -> Binary.Put
putCommentMetadata = putMap putMetadata
  where
    putMetadata :: CommentMetadata -> Binary.Put
    putMetadata (CommentMetadata createdAt replyTo) = do
      putUTCTime createdAt
      putReplyId replyTo

getCommentMetadata :: Binary.Get (Map CommentId CommentMetadata)
getCommentMetadata = getMap getMetadata
  where
    getMetadata :: Binary.Get CommentMetadata
    getMetadata = do
      createdAt <- getUTCTime
      replyTo <- getReplyId
      pure CommentMetadata{Comment.createdAt, Comment.replyTo}

putReplyId :: ReplyId -> Binary.Put
putReplyId (ReplyTask taskId) = Binary.put (0 :: Word8) <> Binary.put taskId
putReplyId (ReplyComment commentId) = Binary.put (1 :: Word8) <> Binary.put commentId

getReplyId :: Binary.Get ReplyId
getReplyId = do
  tag <- Binary.get @Word8
  case tag of
    0 -> ReplyTask <$> Binary.get
    1 -> ReplyComment <$> Binary.get
    _ -> fail $ "invalid ReplyId tag: " ++ show tag

putUTCTime :: UTCTime -> Binary.Put
putUTCTime utcTime = do
  Binary.put @Milli
    (realToFrac @Pico @Milli . nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds utcTime)

getUTCTime :: Binary.Get UTCTime
getUTCTime = do
  millis <- Binary.get @Milli
  pure . posixSecondsToUTCTime . secondsToNominalDiffTime $ realToFrac @Milli @Pico millis

putHistory :: Map StateId Commit -> Binary.Put
putHistory = putMap putCommit

getHistory :: Binary.Get (Map StateId Commit)
getHistory = getMap getCommit

putCommit :: Commit -> Binary.Put
putCommit (Commit parents change) = do
  Binary.put $ Set.toAscList parents
  putChange change

getCommit :: Binary.Get Commit
getCommit = do
  parents <- getSet
  change <- getChange
  pure Commit{parents, change}

putChange :: Change -> Binary.Put
putChange change = do
  putChangeTag change
  putChangeValue change

putChangeTag :: Change -> Binary.Put
putChangeTag NewTask{} = Binary.put @Word64 0
putChangeTag UpdateTask{} = Binary.put @Word64 1
putChangeTag NewComment{} = Binary.put @Word64 2
putChangeTag UpdateComment{} = Binary.put @Word64 3

putChangeValue :: Change -> Binary.Put
putChangeValue (NewTask task) =
  bfoldMapC @Binary (\(Identity a) -> Binary.put a) task
putChangeValue (UpdateTask taskId task) = do
  Binary.put taskId
  bfoldMapC @Binary putUpdate task
putChangeValue (NewComment replyTo comment) = do
  putReplyId replyTo
  bfoldMapC @Binary (\(Identity a) -> Binary.put a) comment
putChangeValue (UpdateComment commentId comment) = do
  Binary.put commentId
  bfoldMapC @Binary putUpdate comment

getChange :: Binary.Get Change
getChange = do
  tag <- Binary.get @Word64
  case tag of
    0 -> do
      task <- btraverse (\Dict -> Identity <$> Binary.get) (bdicts @Binary)
      pure NewTask{task}
    1 -> do
      taskId <- Binary.get
      updateTask <- btraverse (\Dict -> getUpdate) (bdicts @Binary)
      pure UpdateTask{taskId, updateTask}
    2 -> do
      replyTo <- getReplyId
      comment <- btraverse (\Dict -> Identity <$> Binary.get) (bdicts @Binary)
      pure NewComment{replyTo, comment}
    3 -> do
      commentId <- Binary.get
      updateComment <- btraverse (\Dict -> getUpdate) (bdicts @Binary)
      pure UpdateComment{commentId, updateComment}
    _ -> fail $ "invalid Change tag: " ++ show tag

putUpdate :: Binary a => Update a -> Binary.Put
putUpdate None =
  Binary.put @Word64 0
putUpdate (Set a) = do
  Binary.put @Word64 1
  Binary.put a
putUpdate (Pick stateId) = do
  Binary.put @Word64 2
  Binary.put stateId

getUpdate :: Binary a => Binary.Get (Update a)
getUpdate = do
  tag <- Binary.get @Word64
  case tag of
    0 -> pure None
    1 -> Set <$> Binary.get
    2 -> Pick <$> Binary.get
    _ -> fail $ "invalid Update tag: " ++ show tag

data InvalidHeaderException = InvalidHeaderException FilePath
  deriving (Show, Exception)

data UnexpectedVersion = UnexpectedVersion FilePath ByteString
  deriving (Show, Exception)

stateThread :: State -> ReplyId -> [Tree (CommentId, CommentMetadata, Comment (Map StateId))]
stateThread state = go
  where
    go :: ReplyId -> [Tree (CommentId, CommentMetadata, Comment (Map StateId))]
    go target =
      let
        found =
          [ (commentId, metadata, comment)
          | (commentId, metadata) <- Map.toList $ commentMetadata state
          , Comment.replyTo metadata == target
          , comment <- maybeToList $ Map.lookup commentId (comments state)
          ]
      in
        fmap (\x@(commentId, _, _) -> Tree.Node x $ go (ReplyComment commentId)) found
