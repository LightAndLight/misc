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
    waiRoutes
  , waiRoutesCustom

    -- * Routes
  , Routes

    -- ** Checking
  , checkRoutes
  , CheckedRoutes (..)
  , hoistCheckedRoutes
  , toWaiApp
  , toWaiAppCustom

    -- ** Errors
  , RoutesError (..)
  , RouteOverlap (..)

    -- * HTTP Methods
  , Lib.get
  , post
  , Lib.put
  , patch
  , delete
  , method
  , Method (..)

    -- * URL paths
  , Path
  , Part

    -- ** Path operations

    -- *** Joining
  , type (//)
  , (//)

    -- *** Termination
  , End
  , end

    -- ** Path segments
  , str

    -- ** Path parameters
  , IsParam (..)
  , Param
  , param

    -- * Request handlers
  , HandlerFor
  , HandlerT (..)
  , askRequest
  , throwResponse

    -- ** Request bodies
  , body
  , bodyChoice

    -- *** Request body decoders
  , BodyDecoder(..)
  , formUrlEncoded

    -- ** Responses
  , Response(..)
  , response
  , withStatus
  , withHeaders
  , withBody

    -- *** Response body encoders
  , html
  , text
  , BodyEncoder(..)

    -- *** Conversion to WAI responses
  , WaiResponse
  , toWaiResponse

    -- * Links
  , LinkFor
  , link
  , relative
  , mkLink

    -- * Sitemaps
  , sitemap

    -- * 'RouteMap'
  , RouteMap
  , empty
  , Lib.insert
  , match
  , MatchResult (..)
  , MatchedHandler
  , getMatchedHandler
  , hoistMatchedHandler
  , foldMapPaths

    -- * WAI re-exports
    -- ** Common
  , module WaiCommon

    -- ** Requests
  , module Wai.Request
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
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
import Data.Text.Encoding (encodeUtf8)
import Network.Wai as WaiCommon (Application)
import qualified Network.Wai as Wai
import Text.Read (readMaybe)
import Type.Reflection (TypeRep, Typeable, eqTypeRep, typeRep, type (:~~:) (..))
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import Numeric (readHex)
import Data.Char (chr)
import Data.Traversable (for)
import Data.Foldable (foldr', foldl')
import Data.List.NonEmpty (NonEmpty)
import Data.Text.Lazy (LazyText)
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import Network.HTTP.Types.Header (HeaderName)
import Data.Function ((&))
import Wai.Request

newtype Routes handler a
  = Routes (StateT (RouteMap (MatchedHandler handler)) (Either RoutesError) a)
  deriving newtype (Functor, Applicative, Monad)

newtype MatchedHandler m = MatchedHandler {getMatchedHandler :: m Response}

hoistMatchedHandler :: (forall x. m x -> n x) -> MatchedHandler m -> MatchedHandler n
hoistMatchedHandler f (MatchedHandler ma) = MatchedHandler (f ma)

defaultMethodNotAllowed :: HandlerT IO Response
defaultMethodNotAllowed = pure $ response & withStatus 405 & withBody text "method not allowed"

defaultNotFound :: HandlerT IO Response
defaultNotFound = pure $ response & withStatus 404 & withBody text "not found"

{-| Create a WAI 'Application' from some 'Routes'.

Throws a 'RoutesError' exception if checking ('checkRoutes') fails.
-}
waiRoutes :: Routes (HandlerT IO) () -> IO WaiCommon.Application
waiRoutes = waiRoutesCustom defaultMethodNotAllowed defaultNotFound

{-| Create a WAI 'Application' from some 'Routes', with custom error responses.

Throws a 'RoutesError' exception if checking ('checkRoutes') fails.
-}
waiRoutesCustom ::
  -- | "Method not allowed" handler
  HandlerT IO Response ->
  -- | "Not found" handler
  HandlerT IO Response ->
  Routes (HandlerT IO) () ->
  IO WaiCommon.Application
waiRoutesCustom methodNotAllowed notFound =
  either throwIO (pure . toWaiAppCustom methodNotAllowed notFound) . checkRoutes

-- | A set of non-overlapping routes.
newtype CheckedRoutes m = CheckedRoutes {getCheckedRoutes :: RouteMap (MatchedHandler m)}

hoistCheckedRoutes :: (forall x. m x -> n x) -> CheckedRoutes m -> CheckedRoutes n
hoistCheckedRoutes f (CheckedRoutes routeMap) = CheckedRoutes (fmap (hoistMatchedHandler f) routeMap)

checkRoutes :: Routes m () -> Either RoutesError (CheckedRoutes m)
checkRoutes (Routes ma) =
  case runStateT ma empty of
    Left err -> Left err
    Right ((), routeMap) -> Right $ CheckedRoutes routeMap

toWaiApp :: CheckedRoutes (HandlerT IO) -> WaiCommon.Application
toWaiApp = toWaiAppCustom defaultMethodNotAllowed defaultNotFound

toWaiAppCustom ::
  -- | "Method not allowed" handler
  HandlerT IO Response ->
  -- | "Not found" handler
  HandlerT IO Response ->
  CheckedRoutes (HandlerT IO) ->
  WaiCommon.Application
toWaiAppCustom methodNotAllowed notFound (CheckedRoutes routeMap) = go
  where
    go request respond = do
      let
        handler = case match (Method $ Wai.requestMethod request) (Wai.pathInfo request) routeMap of
          MethodNotAllowed -> methodNotAllowed
          NotFound -> notFound
          Matched (MatchedHandler a) -> a
      unHandlerT handler request (respond . toWaiResponse) (respond . toWaiResponse)

data Path :: Type -> Type where
  PathEnd :: Path End
  PathCons :: Part a -> Path b -> Path (a // b)

data Part :: Type -> Type where
  PartStr :: !Text -> Part ()
  PartParam :: IsParam a => !Text -> Part (Param a)

{-| A static 'Path' piece.

e.g. @"posts" '//' 'param' "id" \@'Int'@

See also: 'str'
-}
instance a ~ () => IsString (Part a) where
  fromString = str

{-| A static 'Path' piece.

See also: 'IsString' instance for 'Part'
-}
str :: String -> Part ()
str = PartStr . fromString

data Param a

class Typeable a => IsParam a where
  parseParam :: Text -> Maybe a
  printParam :: a -> Text

instance IsParam Int where
  parseParam = readMaybe . Text.unpack
  printParam = Text.pack . show

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

newtype HandlerT m a
  = HandlerT
  { unHandlerT ::
      forall r.
      Wai.Request ->
      (Response -> m r) ->
      (a -> m r) ->
      m r}

instance Functor m => Functor (HandlerT m) where
  fmap f (HandlerT ma) = HandlerT $ \req err done ->
    ma req err (done . f)

instance Applicative m => Applicative (HandlerT m) where
  pure a = HandlerT $ \_req _err done  -> done a
  (<*>) (HandlerT mf) (HandlerT ma) = HandlerT $ \req err done ->
    mf req err (\f -> ma req err (done . f))

instance Monad m => Monad (HandlerT m) where
  (>>=) (HandlerT ma) f = HandlerT $ \req err done ->
    ma req err (\a -> unHandlerT (f a) req err done)

instance MonadIO m => MonadIO (HandlerT m) where
  liftIO ma = HandlerT $ \_req _err done ->
    done =<< liftIO ma

instance MonadTrans HandlerT where
  lift ma = HandlerT $ \_req _err done -> ma >>= done

askRequest :: Monad m => HandlerT m Wai.Request
askRequest = HandlerT $ \req _err done -> done req

throwResponse :: Response -> HandlerT m a
throwResponse res = HandlerT $ \_req err _done -> err res

type family HandlerFor m a where
  HandlerFor m End = m Response
  HandlerFor m (Param a -> b) = a -> HandlerFor m b

newtype Handler m a = Handler {getHandler :: HandlerFor m a}

-- | A HTTP method
newtype Method = Method {unMethod :: ByteString}
  deriving newtype (Show, Eq, Ord, IsString)

method :: Method -> Path a -> HandlerFor m a -> Routes m ()
method m path handler = Routes $ do
  s <- Control.Monad.Trans.State.Strict.get
  case insert m path handler s of
    Left err -> lift $ Left err
    Right s' -> Control.Monad.Trans.State.Strict.put s'

get :: Path a -> HandlerFor m a -> Routes m ()
get = method "GET"

post :: Path a -> HandlerFor m a -> Routes m ()
post = method "POST"

put :: Path a -> HandlerFor m a -> Routes m ()
put = method "PUT"

patch :: Path a -> HandlerFor m a -> Routes m ()
patch = method "PATCH"

delete :: Path a -> HandlerFor m a -> Routes m ()
delete = method "DELETE"

data RouteMap (a :: Type)
  = RouteMap
  { routeMapMethods :: !(Map Method a)
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
  = DuplicateRoute {duplicateRouteMethod :: Method, duplicateRoutePath :: Text}
  | OverlappingRoutes [RouteOverlap]
  deriving (Show, Eq, Exception)

data RouteOverlap
  = RouteOverlap
  { routeOverlappedPrefix :: Text
  , routeOverlappingPrefix :: Text
  }
  deriving (Show, Eq)

insert ::
  forall a m.
  -- | &#xA0;
  Method ->
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
      Method ->
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
    go prefix method'' (PathCons (PartStr s) rest) handler routeMap =
      case Map.lookup s (routeMapStatic routeMap) of
        Just inner -> do
          inner' <- go (prefix <> "/" <> s) method'' rest handler inner
          pure routeMap{routeMapStatic = Map.insert s inner' (routeMapStatic routeMap)}
        Nothing ->
          case routeMapDynamic routeMap of
            Just (Matcher (_ :: TypeRep x) mName _mRoutes) ->
              case parseParam @x s of
                Nothing -> do
                  inner' <- go (prefix <> "/" <> s) method'' rest handler empty
                  pure routeMap{routeMapStatic = Map.insert s inner' (routeMapStatic routeMap)}
                Just{} ->
                  Left $
                    OverlappingRoutes
                      [ RouteOverlap
                          { routeOverlappedPrefix = prefix <> "/{" <> mName <> "}"
                          , routeOverlappingPrefix = prefix <> "/" <> s
                          }
                      ]
            Nothing -> do
              inner' <- go (prefix <> "/" <> s) method'' rest handler empty
              pure routeMap{routeMapStatic = Map.insert s inner' (routeMapStatic routeMap)}
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
                  { routeOverlappedPrefix = prefix <> "/{" <> mName <> "}"
                  , routeOverlappingPrefix = prefix <> "/{" <> name <> "}"
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
                        { routeOverlappedPrefix = prefix <> "/" <> parsedKey
                        , routeOverlappingPrefix = prefix <> "/{" <> name <> "}"
                        }
                  )
                  parsedKeys

data MatchResult m
  = Matched (MatchedHandler m)
  | MethodNotAllowed
  | NotFound

match ::
  Method ->
  -- | URL path parts
  [Text] ->
  RouteMap (MatchedHandler m) ->
  MatchResult m
match = go id
  where
    go :: (x -> MatchedHandler m) -> Method -> [Text] -> RouteMap x -> MatchResult m
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

data SomePath where
  SomePath :: Path x -> SomePath

foldMapPaths ::
  Monoid m =>
  (forall x. Method -> Path x -> m) ->
  RouteMap a ->
  m
foldMapPaths = go SomePath
  where
    go ::
      Monoid m =>
      (forall x. Path x -> SomePath) ->
      (forall x. Method -> Path x -> m) ->
      RouteMap a ->
      m
    go prefix f routeMap =
      foldMap
        ( \m ->
            case prefix PathEnd of
              SomePath path -> f m path
        )
        (Map.keys $ routeMapMethods routeMap)
        <> Map.foldMapWithKey
          ( \k ->
              go
                (prefix . PathCons (PartStr k))
                f
          )
          (routeMapStatic routeMap)
        <> foldMap
          ( \(Matcher (_mType :: TypeRep x) mName mRoutes) ->
              go
                (prefix . PathCons (PartParam @x mName))
                f
                mRoutes
          )
          (routeMapDynamic routeMap)

displayPath :: Path a -> Text
displayPath PathEnd = ""
displayPath (PathCons part PathEnd) = displayPart part
displayPath (PathCons part rest@PathCons{}) = displayPart part <> "/" <> displayPath rest

displayPart :: Part a -> Text
displayPart (PartStr s) = s
displayPart (PartParam name) = "{" <> name <> "}"

type family LinkFor r a where
  LinkFor r End = r
  LinkFor r (Param a -> b) = a -> LinkFor r b

mkLink :: (Text -> r) -> Path a -> LinkFor r a
mkLink acc PathEnd = acc ""
mkLink acc (PathCons (PartStr s) PathEnd) = acc s
mkLink acc (PathCons (PartParam _name) PathEnd) = acc . printParam
mkLink acc (PathCons (PartStr s) rest@PathCons{}) = mkLink (\suffix -> acc (s <> "/" <> suffix)) rest
mkLink acc (PathCons (PartParam _name) rest@PathCons{}) = \x -> mkLink (\suffix -> acc (printParam x <> "/" <> suffix)) rest

relative :: Path a -> LinkFor Text a
relative = mkLink id

link :: Path a -> LinkFor Text a
link = mkLink ("/" <>)

sitemap :: RouteMap m -> [ByteString]
sitemap = foldMapPaths f
  where
    f m path = [unMethod m <> " /" <> encodeUtf8 (displayPath path)]

data BodyDecoder a
  = BodyDecoder
  { bodyDecoderContentType :: ByteString
  , bodyDecoderDecode :: Lazy.ByteString -> Maybe a
  } deriving (Functor)

formUrlEncoded :: BodyDecoder (Map Text [Text])
formUrlEncoded =
  BodyDecoder
  { bodyDecoderContentType = "application/x-www-form-urlencoded"
  , bodyDecoderDecode =
      \input -> do
      let parts = ByteString.Lazy.Char8.split '&' input
      pairs <- for parts $ \part -> do
        case ByteString.Lazy.Char8.split '=' part of
          [k, v] -> (,) <$> percentDecode k <*> percentDecode v
          _ -> Nothing
      pure $
        foldr'
          (\(k, v) -> Map.insertWith (++) k [v])
          Map.empty
          pairs
  }
  where
    percentDecode =
      fmap Text.pack . go . ByteString.Lazy.Char8.unpack
      where
        go [] = Just []
        go ('+' : rest) = (' ' :) <$> go rest
        go ('%' : a : b : rest) =
          case readHex [a, b] of
            [(n, "")] -> (chr n :) <$> go rest
            _ -> Nothing
        go (a : rest) = (a :) <$> go rest

body :: BodyDecoder a -> HandlerT IO a
body = bodyChoice . pure

bodyChoice :: NonEmpty (BodyDecoder a) -> HandlerT IO a
bodyChoice choices = do
  request <- askRequest
  case lookup "Content-Type" $ requestHeaders request of
    Nothing ->
      -- TODO: overload
      throwResponse $
        response
          & withStatus 406
          & withBody text "not acceptable"
    Just contentType -> do
      let
        decoder =
          foldr1
            (\choice rest ->
              if bodyDecoderContentType choice == contentType
              then choice
              else rest
            )
            choices
      bodyContent <- liftIO $ strictRequestBody request
      case bodyDecoderDecode decoder bodyContent of
        Nothing ->
          throwResponse $
            response
              & withStatus 400
              & withBody text "bad request"
        Just a -> pure a

data BodyEncoder a
  = BodyEncoder
  { bodyEncoderContentType :: ByteString
  , bodyEncoderEncoder :: a -> Lazy.ByteString
  }

html :: BodyEncoder LazyText
html =
  BodyEncoder
    { bodyEncoderContentType = "text/html;charset=UTF-8"
    , bodyEncoderEncoder = Text.Lazy.Encoding.encodeUtf8
    }

text :: BodyEncoder LazyText
text =
  BodyEncoder
    { bodyEncoderContentType = "text/plain;charset=UTF-8"
    , bodyEncoderEncoder = Text.Lazy.Encoding.encodeUtf8
    }

data Response
  = Response
  { responseStatus :: !Int
  , responseHeaders :: !(Map HeaderName ByteString)
  , responseBody :: !Lazy.ByteString
  } deriving (Show, Eq)

type WaiResponse = Wai.Response

toWaiResponse :: Response -> WaiResponse
toWaiResponse res =
  Wai.responseLBS
    (toEnum $ responseStatus res)
    (Map.toList $ responseHeaders res)
    (responseBody res)

response :: Response
response =
  Response
    { responseStatus = 204
    , responseHeaders = mempty
    , responseBody = mempty
    }

withBody :: BodyEncoder a -> a -> Response -> Response
withBody encoder input res =
  res
    { responseStatus = if oldStatus == 204 then 200 else oldStatus
    , responseHeaders = Map.insert "Content-Type" (bodyEncoderContentType encoder) (responseHeaders res)
    , responseBody = encoded
    }
  where
    oldStatus = responseStatus res
    encoded = bodyEncoderEncoder encoder input

withHeaders :: Foldable f => f (HeaderName, ByteString) -> Response -> Response
withHeaders headers res =
  res
    {responseHeaders =
      foldl'
        (\acc (k, v) -> Map.insert k v acc)
        (responseHeaders res)
        headers
    }

withStatus :: Int -> Response -> Response
withStatus status res =
  res{responseStatus = status}
