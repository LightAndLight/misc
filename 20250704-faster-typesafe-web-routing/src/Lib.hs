{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( -- * WAI applications
    module Network.Wai
  , wai
  , waiCustom
  , Routes

    -- ** Errors
  , RoutesError (..)
  , RouteOverlap (..)

    -- * HTTP Methods
  , Lib.get
  , post
  , Lib.put
  , method

    -- * URL paths
  , Path
  , Part

    -- ** Join paths
  , type (//)
  , (//)

    -- ** Terminate paths
  , End
  , end

    -- ** Path parameters
  , IsParam (..)
  , Param
  , param

    -- * Request handlers
  , HandlerFor
  , HandlerT(..)
  , askRequest

    -- * 'RouteMap'
  , RouteMap
  , empty
  , Lib.insert
  , match
  , MatchResult(..)
  , MatchedHandler
  , getMatchedHandler
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Control.Monad.Trans.State.Strict (StateT, get, put, runStateT)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Types.Status (methodNotAllowed405, notFound404)
import Network.Wai
import qualified Network.Wai as Wai
import Text.Read (readMaybe)
import Type.Reflection (TypeRep, Typeable, eqTypeRep, typeRep, type (:~~:) (..))
import Control.Monad.IO.Class (MonadIO)

newtype Routes m a
  = Routes (StateT (RouteMap (MatchedHandler m)) (Either RoutesError) a)
  deriving newtype (Functor, Applicative, Monad)

newtype MatchedHandler m = MatchedHandler{ getMatchedHandler :: HandlerT m Response }

wai :: Routes IO () -> IO Application
wai = either throwIO pure . waiCustom methodNotAllowed notFound
  where
    methodNotAllowed :: HandlerT IO Wai.Response
    methodNotAllowed = pure $ Wai.responseLBS methodNotAllowed405 [] "method not allowed"

    notFound :: HandlerT IO Wai.Response
    notFound = pure $ Wai.responseLBS notFound404 [] "not found"

waiCustom ::
  -- | "Method not allowed" handler
  HandlerT IO Wai.Response ->

  -- | "Not found" handler
  HandlerT IO Wai.Response ->
  Routes IO () ->
  Either RoutesError Application
waiCustom methodNotAllowed notFound (Routes ma) =
  case runStateT ma empty of
    Left err -> Left err
    Right ((), routeMap) ->
      Right $
        \request respond -> do
          let
            handler = case match (Wai.requestMethod request) (Wai.pathInfo request) routeMap of
              MethodNotAllowed -> methodNotAllowed
              NotFound -> notFound
              Matched (MatchedHandler a) -> a
          respond =<< runReaderT (runHandlerT handler) request

data Path :: Type -> Type where
  PathEnd :: Path End
  PathCons :: Part a -> Path b -> Path (a // b)

data Part :: Type -> Type where
  PartStr :: !Text -> Part ()
  PartParam :: IsParam a => !Text -> Part (Param a)

{-| A static 'Path' piece.

e.g. @"posts" '//' 'param' "id" \@'Int'@
-}
instance a ~ () => IsString (Part a) where
  fromString = PartStr . fromString

data Param a

class Typeable a => IsParam a where
  parseParam :: Text -> Maybe a

instance IsParam Int where
  parseParam = readMaybe . Text.unpack

param :: String -> forall a. IsParam a => Part (Param a)
param = PartParam . fromString

type family (//) a b where
  () // a = a
  Param a // b = Param a -> b

(//) :: Part a -> Path b -> Path (a // b)
(//) = PathCons

infixr 5 //

data End

end :: Path End
end = PathEnd

newtype HandlerT m a = HandlerT {runHandlerT :: ReaderT Wai.Request m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)

askRequest :: Monad m => HandlerT m Wai.Request
askRequest = HandlerT ask

type family HandlerFor m a where
  HandlerFor m End = HandlerT m Wai.Response
  HandlerFor m (Param a -> b) = a -> HandlerFor m b

newtype Handler m a = Handler {getHandler :: HandlerFor m a}

method :: ByteString -> Path a -> HandlerFor m a -> Routes m ()
method name path handler = Routes $ do
  s <- Control.Monad.Trans.State.Strict.get
  case insert name path handler s of
    Left err -> lift $ Left err
    Right s' -> Control.Monad.Trans.State.Strict.put s'

get :: Path a -> HandlerFor m a -> Routes m ()
get = method "GET"

post :: Path a -> HandlerFor m a -> Routes m ()
post = method "POST"

put :: Path a -> HandlerFor m a -> Routes m ()
put = method "PUT"

data RouteMap (a :: Type)
  = RouteMap
  { routeMapMethods :: !(Map ByteString a)
  , routeMapStatic :: !(Map Text (RouteMap a))
  , routeMapDynamic :: Maybe (Matcher a)
  }
  deriving (Functor)

data Matcher (a :: Type) where
  Matcher ::
    IsParam x =>
    TypeRep x ->
    !Text ->
    RouteMap (x -> a) ->
    Matcher a

deriving instance Functor Matcher

empty :: RouteMap a
empty = RouteMap mempty mempty Nothing

data RoutesError
  = DuplicateRoute {duplicateRouteMethod :: ByteString, duplicateRoutePath :: Text}
  | OverlappingRoutes [RouteOverlap]
  deriving (Show, Exception)

data RouteOverlap
  = RouteOverlap
  { routeOverlapCurrent :: Text
  , routeOverlapNew :: Text
  }
  deriving (Show)

insert ::
  forall a m.
  -- | HTTP method
  ByteString ->
  Path a ->
  HandlerFor m a ->
  RouteMap (MatchedHandler m) ->
  Either RoutesError (RouteMap (MatchedHandler m))
insert m p h =
  coerce . go @Identity mempty m p (Identity (Handler @m h)) . coerce
  where
    go ::
      forall f a'.
      Functor f =>
      Text ->
      ByteString ->
      Path a' ->
      f (Handler m a') ->
      RouteMap (f (Handler m End)) ->
      Either RoutesError (RouteMap (f (Handler m End)))
    go prefix method'' PathEnd value routeMap =
      case Map.lookup method'' (routeMapMethods routeMap) of
        Nothing ->
          Right
            routeMap
              { routeMapMethods =
                  Map.insert
                    method''
                    value
                    (routeMapMethods routeMap)
              }
        Just{} -> Left $ DuplicateRoute method'' prefix
    go prefix method'' (PathCons (PartStr str) rest) handler routeMap =
      case Map.lookup str (routeMapStatic routeMap) of
        Just inner -> do
          inner' <- go (prefix <> "/" <> str) method'' rest handler inner
          pure routeMap{routeMapStatic = Map.insert str inner' (routeMapStatic routeMap)}
        Nothing ->
          case routeMapDynamic routeMap of
            Just (Matcher (_ :: TypeRep x) mName _mRoutes) ->
              case parseParam @x str of
                Nothing -> do
                  inner' <- go (prefix <> "/" <> str) method'' rest handler empty
                  pure routeMap{routeMapStatic = Map.insert str inner' (routeMapStatic routeMap)}
                Just{} ->
                  Left $
                    OverlappingRoutes
                      [RouteOverlap{routeOverlapCurrent = prefix <> "/{" <> mName <> "}", routeOverlapNew = prefix <> "/" <> str}]
            Nothing -> do
              inner' <- go (prefix <> "/" <> str) method'' rest handler empty
              pure routeMap{routeMapStatic = Map.insert str inner' (routeMapStatic routeMap)}
    go prefix method'' (PathCons (PartParam @x name) (rest :: Path b)) handler routeMap
      | Just (Matcher mType mName mRoutes) <- routeMapDynamic routeMap =
          case eqTypeRep mType (typeRep @x) of
            Just HRefl -> do
              mRoutes' <-
                go
                  @(Compose ((->) x) f)
                  (prefix <> "/{" <> name <> "}")
                  method''
                  rest
                  (Compose $ \x -> fmap (Handler @m @b . ($ x) . getHandler) handler)
                  (coerce mRoutes)
              Right
                routeMap
                  { routeMapDynamic =
                      Just $ Matcher mType name (coerce mRoutes')
                  }
            Nothing ->
              Left . OverlappingRoutes . pure $
                RouteOverlap
                  { routeOverlapCurrent = prefix <> "/{" <> mName <> "}"
                  , routeOverlapNew = prefix <> "/{" <> name <> "}"
                  }
      | otherwise =
          case mapMaybe (\key -> key <$ parseParam @x key) . Map.keys $ routeMapStatic routeMap of
            [] -> do
              mRoutes <-
                go
                  @(Compose ((->) x) f)
                  (prefix <> "/{" <> name <> "}")
                  method''
                  rest
                  (Compose $ \x -> fmap (Handler @m @b . ($ x) . getHandler) handler)
                  empty
              Right
                routeMap
                  { routeMapDynamic =
                      Just $ Matcher (typeRep @x) name (coerce mRoutes)
                  }
            parsedKeys ->
              Left . OverlappingRoutes $
                fmap
                  ( \parsedKey ->
                      RouteOverlap
                        { routeOverlapCurrent = prefix <> "/" <> parsedKey
                        , routeOverlapNew = prefix <> "/{" <> name <> "}"
                        }
                  )
                  parsedKeys

data MatchResult m
  = Matched (MatchedHandler m)
  | MethodNotAllowed
  | NotFound

match ::
  -- | HTTP method
  ByteString ->

  -- | URL path parts
  [Text] ->
  RouteMap (MatchedHandler m) ->
  MatchResult m
match = go id
  where
    go :: (x -> MatchedHandler m) -> ByteString -> [Text] -> RouteMap x -> MatchResult m
    go f method' [] routeMap =
      maybe MethodNotAllowed (Matched . f) $ Map.lookup method' (routeMapMethods routeMap)
    go f method' (p : ps) routeMap =
      case Map.lookup p (routeMapStatic routeMap) of
        Just routeMap' -> go f method' ps routeMap'
        Nothing ->
          case routeMapDynamic routeMap of
            Nothing -> NotFound
            Just (Matcher (_ :: TypeRep x) _ mRoutes) ->
              case parseParam @x p of
                Nothing -> NotFound
                Just x -> go (f . ($ x)) method' ps mRoutes
