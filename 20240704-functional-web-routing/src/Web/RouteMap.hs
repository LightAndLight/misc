{-# language GADTs #-}
module Web.RouteMap
  ( toRouteMapUnchecked
  , RouteMap
  , MethodEndpoints(..)
  , SelectedEndpoint(..)
  , root
  , empty
  , singleton
  , insert
  , findPath
  , debugRouteMap
  )
where

import Barbies (ApplicativeB, TraversableB, bfoldMap, bzip)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Type.Equality ((:~~:)(..))
import Type.Reflection (TypeRep, eqTypeRep)
import Web.Endpoint (Endpoint(..), EndpointFor)
import Web.Path (IsPart(..), Part(..), decodePart)
import Web.Route (Route(..), ActionType(..))

data RouteMap a
  = RouteMap
      (Maybe (MethodEndpoints a))
      (Map String (RouteMap a))
      (Maybe (Matcher a))

root :: RouteMap a -> Maybe (MethodEndpoints a)
root (RouteMap mRoot _ _) = mRoot

instance Functor RouteMap where
  fmap f (RouteMap mRoot static dynamic) =
    RouteMap
      (fmap (fmap f) mRoot)
      (fmap (fmap f) static)
      (fmap (fmap f) dynamic)

instance Semigroup (RouteMap x) where
  (<>) (RouteMap mRoot1 static1 dynamic1) (RouteMap mRoot2 static2 dynamic2) =
    RouteMap
      (maybeZipWith
        (\(MethodEndpoints methods1) (MethodEndpoints methods2) ->
          MethodEndpoints $ Map.unionWith (\_l r -> r) methods1 methods2
        )
        mRoot1
        mRoot2
      )
      (Map.unionWith (<>) static1 static2)
      (maybeZipWith
        (\(Matcher _ ty1 m1) (Matcher name2 ty2 m2) ->
          case eqTypeRep ty1 ty2 of
            Nothing ->
              Matcher name2 ty2 m2
            Just HRefl ->
              Matcher name2 ty1 (m1 <> m2)
        )
        dynamic1
        dynamic2
      )
    where
      maybeZipWith :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
      maybeZipWith _ a Nothing = a
      maybeZipWith _ Nothing b = b
      maybeZipWith f (Just a) (Just b) = Just (f a b)

instance Monoid (RouteMap x) where
  mempty = empty

newtype MethodEndpoints a = MethodEndpoints (Map String a)
  deriving (Functor)

data Matcher a where
  Matcher :: IsPart x => String -> TypeRep x -> RouteMap (x -> a) -> Matcher a

instance Functor Matcher where
  fmap f (Matcher name ty m) = Matcher name ty ((fmap . fmap) f m)

empty :: RouteMap a
empty = RouteMap Nothing Map.empty Nothing

data SelectedEndpoint m where
  SelectedEndpoint ::
    ActionType x ->
    EndpointFor m x ->
    SelectedEndpoint m

singleton :: Route a -> Endpoint m a -> RouteMap (SelectedEndpoint m)
singleton route value =
  runIdentity <$> singletonF route (Identity value)

singletonF ::
  forall a m f.
  Functor f =>
  Route a ->
  f (Endpoint m a) ->
  RouteMap (f (SelectedEndpoint m))
singletonF (RouteNil method action) value =
  RouteMap
    (Just . MethodEndpoints . Map.singleton method $
      fmap (\(Endpoint endpoint) -> SelectedEndpoint action endpoint) value
    )
    Map.empty
    Nothing
singletonF (RouteCons part parts) value =
  case part of
    PartString s ->
      RouteMap
        Nothing
        (Map.singleton s $ singletonF parts value)
        Nothing
    PartVar name ty ->
      RouteMap
        Nothing
        Map.empty
        (Just $
          Matcher
            name
            ty
            (getCompose <$>
              singletonF
                parts
                (Compose $ \a -> fmap (\(Endpoint endpoint) -> Endpoint $ endpoint a) value)
            )
        )

insert ::
  Route a ->
  Endpoint m a ->
  RouteMap (SelectedEndpoint m) ->
  RouteMap (SelectedEndpoint m)
insert route value m =
  runIdentity <$> insertF route (Identity value) (fmap Identity m)

insertF ::
  forall a m f.
  Functor f =>
  Route a ->
  f (Endpoint m a) ->
  RouteMap (f (SelectedEndpoint m)) ->
  RouteMap (f (SelectedEndpoint m))
insertF (RouteNil method mBody) value (RouteMap _ static dynamic) =
  RouteMap
    (Just . MethodEndpoints . Map.singleton method $
      fmap (\(Endpoint endpoint) -> SelectedEndpoint mBody endpoint) value
    )
    static
    dynamic
insertF (RouteCons part parts) value (RouteMap mRoot static dynamic) =
  case part of
    PartString s ->
      RouteMap
        mRoot
        (Map.alter
           (Just .
              insertF parts value .
              fromMaybe empty
            )
           s
           static
        )
        dynamic
    PartVar name ty ->
      case dynamic of
        Just (Matcher _ ty' m') | Just HRefl <- eqTypeRep ty ty' ->
          {- The `Part` of the path we're inserting decodes the same type as the `Part`
          at this level of the map. 

          Update the rest of the map, if possible.
          -}
          RouteMap
            mRoot
            static
            (Just $
              Matcher
                name
                ty
                (getCompose <$>
                  insertF
                    parts
                    (Compose $ \a -> fmap (\(Endpoint endpoint) -> Endpoint (endpoint a)) value)
                    (fmap Compose m')
                )
            )
        _ ->
          RouteMap
            mRoot
            static
            (Just $
              Matcher
                name
                ty
                (getCompose <$>
                  singletonF
                    parts
                    (Compose $ \a -> fmap (\(Endpoint endpoint) -> Endpoint (endpoint a)) value)
                )
            )

findPath :: [String] -> RouteMap x -> Maybe (MethodEndpoints x)
findPath = go id
  where
    go :: (x -> x') -> [String] -> RouteMap x -> Maybe (MethodEndpoints x')
    go f [] m =
      fmap (fmap f) (root m)
    go f (x : xs) (RouteMap _ static dynamic) =
      case Map.lookup x static of
        Just m' -> go f xs m'
        Nothing -> do
          Matcher @a _name _ty m' <- dynamic
          a <- decodePart @a x
          go (f . ($ a)) xs m'

toRouteMapUnchecked ::
  (TraversableB b, ApplicativeB b) =>
  b Route ->
  b (Endpoint IO) ->
  RouteMap (SelectedEndpoint IO)
toRouteMapUnchecked routes endpoints =
  bfoldMap
    (\(Pair route endpoint) -> singleton route endpoint)
    (bzip routes endpoints)

debugRouteMap :: RouteMap (SelectedEndpoint m) -> [(String, [String])]
debugRouteMap = go id . fmap Identity
  where
    go ::
      Functor f =>
      ([String] -> [String]) ->
      RouteMap (f (SelectedEndpoint m)) ->
      [(String, [String])]
    go prefix (RouteMap mRoot static dynamic) =
      maybe
        []
        (\(MethodEndpoints methods) ->
          let path = prefix [] in
          fmap (\(method, _) -> (method, path)) . Map.toList $ methods)
        mRoot <>
      Map.foldrWithKey
        (\key value rest -> go (prefix . (:) key) value <> rest)
        (foldMap
          (\(Matcher name _ty m') ->
            go (prefix . (:) ("{" <> name <> "}")) (fmap Compose m')
          )
          dynamic
        )
        static
