{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
module Web.Route
  ( Route
  , route
  , route_
  , RouteType
  , request
  , RouteRequest
  , response
  , RouteResponse
  , routeMethod
  , routePath
  , ParamsFor
  , routeAction
  , ActionTypeFor
  , routeLink
  , sitemap

  -- * Internals
  , Route(..)
  , ActionType(..)
  , RouteRequest(..)
  , RouteResponse(..)
  , RouteTypeImpl
  , SplitRouteType
  , JoinRouteType
  , Fst
  , Snd
  )
where

import Barbies (TraversableB, bfoldMap)
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Kind (Constraint, Type)
import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (TypeError, ErrorMessage(..))
import Web.Body (Body)
import Web.Link (Link(..))
import Web.Param (Param)
import Web.Path (Path(..), Part(..), encodePart)
import Web.UrlTemplate (pathToUrlTemplate)
import Web.Request (IsRequest, requestContentType)
import Web.Response (IsResponse, Response)

type family RouteCons (a :: Type) (b :: Type) :: Type where
  RouteCons () a = a
  RouteCons (Param a) b = Param a -> b

data Route a where
  RouteNil :: String -> ActionType a -> Route a
  RouteCons :: Part a -> Route b -> Route (RouteCons a b)

data ActionType a where
  WithBody :: (IsRequest a, IsResponse b) => ActionType (Body a -> Response b)
  NoBody :: IsResponse a => ActionType (Response a)

data RouteRequest a where
  RouteRequest :: IsRequest a => RouteRequest a

request :: IsRequest a => RouteRequest a
request = RouteRequest

data RouteResponse a where
  RouteResponse :: IsResponse a => RouteResponse a

response :: IsResponse a => RouteResponse a
response = RouteResponse

type family JoinRouteType (as :: [Type]) (b :: Type) :: Type where
  JoinRouteType '[] b = b
  JoinRouteType (Param a ': as) b = Param a -> JoinRouteType as b

type family Fst (p :: (a, b)) :: a where Fst '(a, b) = a
type family Snd (p :: (a, b)) :: b where Snd '(a, b) = b

type family SplitRouteType (a :: Type) :: ([Type], Type) where
  SplitRouteType (Param a -> b) = '( Param a ': Fst (SplitRouteType b), Snd (SplitRouteType b))
  SplitRouteType (Body a -> Response b) = '( '[], Body a -> Response b)
  SplitRouteType (Response a) = '( '[], Response a)

class PathMismatch (a :: [Type]) (b :: [Type])
instance
  TypeError
    (Text "Type mismatch in route path" :$$:
      (Text "Expected: " :<>: ShowType (Path b)) :$$:
      (Text "Actual: " :<>: ShowType (Path a))
    ) =>
  PathMismatch a b

class ActionMismatch (a :: Type) (b :: Type)
instance
  TypeError
    (Text "Type mismatch in route action" :$$:
      (Text "Expected: " :<>: ShowType b) :$$:
      (Text "Actual: " :<>: ShowType a)
    ) =>
  ActionMismatch a b

class Matches (c :: x -> y -> Constraint) (a :: x) (b :: y) where

instance {-# overlappable #-} c a b => Matches c a b where
instance Matches c a a where

type RouteType params action r = RouteTypeImpl params action r

type RouteTypeImpl params action r =
  ( r ~ JoinRouteType params action
  , Matches PathMismatch params (Fst (SplitRouteType r))
  , Matches ActionMismatch action (Snd (SplitRouteType r))
  )

route ::
  RouteType params (Body a -> Response b) r =>
  String -> 
  Path params ->
  RouteRequest a ->
  RouteResponse b ->
  Route r
route = go
  where
    go ::
      ( r ~ JoinRouteType params action
      , action ~ (Body a -> Response b)
      ) =>
      String -> 
      Path params ->
      RouteRequest a ->
      RouteResponse b ->
      Route r
    go method PathNil RouteRequest RouteResponse =
      RouteNil method WithBody
    go method (PathCons part parts) req@RouteRequest res@RouteResponse =
      case part of
        PartString{} ->
          RouteCons part (go method parts req res)
        PartVar{} ->
          RouteCons part (go method parts req res)

route_ ::
  RouteType params (Response a) r =>
  String -> 
  Path params ->
  RouteResponse a ->
  Route r
route_ = go
   where
    go ::
      r ~ JoinRouteType params (Response a) =>
      String -> 
      Path params ->
      RouteResponse a ->
      Route r
    go method PathNil RouteResponse =
      RouteNil method NoBody
    go method (PathCons part parts) res@RouteResponse =
      case part of
        PartString{} ->
          RouteCons part (go method parts res)
        PartVar{} ->
          RouteCons part (go method parts res)

type family ParamsFor (a :: Type) :: [Type] where
  ParamsFor (Param a -> b) = Param a ': ParamsFor b
  ParamsFor (Body a -> Response b) = '[]
  ParamsFor (Response a) = '[]

type family ActionTypeFor (a :: Type) :: Type where
  ActionTypeFor (Param a -> b) = ActionTypeFor b
  ActionTypeFor (Body a -> Response b) = Body a -> Response b
  ActionTypeFor (Response a) = Response a

withRoute ::
  forall a r.
  Route a ->
  (String -> Path (ParamsFor a) -> ActionType (ActionTypeFor a) -> r) ->
  r
withRoute = go id
  where
    go ::
      (Path (ParamsFor x) -> Path (ParamsFor a)) ->
      Route x ->
      (String -> Path (ParamsFor a) -> ActionType (ActionTypeFor x) -> r) ->
      r
    go acc (RouteNil method action) f =
      case action of
        WithBody -> f method (acc PathNil) action
        NoBody -> f method (acc PathNil) action
    go acc (RouteCons part rest) f =
      case part of
        PartString{} -> go (acc . PathCons part) rest f
        PartVar{} -> go (acc . PathCons part) rest f

routeMethod :: Route a -> String
routeMethod route = withRoute route (\method _ _ -> method)

routePath :: Route a -> Path (ParamsFor a)
routePath route = withRoute route (\_ path _ -> path)

routeAction :: Route a -> ActionType (ActionTypeFor a)
routeAction route = withRoute route (\_ _ action -> action)

data RouteView a
  = RouteView String (Path (ParamsFor a)) (ActionType (ActionTypeFor a))

viewRoute :: Route a -> RouteView a
viewRoute route = withRoute route RouteView

routeLink :: Route a -> Link a
routeLink (RouteNil _ action) =
  case action of
    WithBody ->
      Link $ \f -> f []
    NoBody ->
      Link $ \f -> f []
routeLink (RouteCons part parts) =
  case part of
    PartString s ->
      let Link l = routeLink parts in
      Link $ \f -> l (f . (:) s)
    PartVar _name _ty ->
      let Link l = routeLink parts in
      Link $ \f a -> l (f . (:) (encodePart a))

sitemap :: TraversableB b => b Route -> [String]
sitemap =
  bfoldMap
    (\route ->
      withRoute route $ \method path action ->
      let parts = pathToUrlTemplate path in
      [ method <>
          " " <>
          (case action of
            WithBody @b ->
              "(" <> ByteString.Char8.unpack (requestContentType (Proxy :: Proxy b)) <> ") "
            NoBody -> ""
          ) <>
          intercalate "/" parts
      ]
    )
