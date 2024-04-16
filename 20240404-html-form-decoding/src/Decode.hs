{-# LANGUAGE DerivingVia #-}
module Decode where

import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.Map as Map
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.List (stripPrefix)
import Data.Functor.Identity (Identity(..))
import Prelude hiding (map)
import Barbies (FunctorB, bmap)
import Data.Functor.Compose (Compose (..))
import Control.Monad.State (StateT(..))
import Data.Bifunctor (first)
import qualified Data.Tuple as Tuple

data DecodeError
  = KeyNotFound String
  | TooManyValues String [String]
  | DecodeFailure String String
  | UnexpectedInput (Map String (NonEmpty String))
  | NotEnoughValues Int (NonEmpty String)
  deriving Show

newtype Decoder a 
  = Decoder
  { runDecoder :: 
      Map String (NonEmpty String) ->
      Either DecodeError (a, Map String (NonEmpty String)) 
  }
  deriving (Functor, Applicative)
  via StateT (Map String (NonEmpty String)) (Either DecodeError)

decode :: 
  Decoder a ->
  Map String (NonEmpty String) ->
  Either DecodeError (Map String (NonEmpty String), a) 
decode = (fmap . fmap) Tuple.swap . runDecoder

type SimpleDecoder a = String -> Maybe a

string :: SimpleDecoder String
string input =
  pure input

int :: SimpleDecoder Int
int input =
  readMaybe @Int input

simple :: String -> SimpleDecoder a -> Decoder a
simple key dec =
  Decoder $ \input ->
  case Map.lookup key input of
    Nothing ->
      Left $ KeyNotFound key
    Just (value :| values) -> do
      case dec value of
        Nothing ->
          Left $ DecodeFailure key value
        Just a -> do
          let 
            input' =
              case nonEmpty values of
                Just values' ->
                  Map.insert key values' input
                Nothing ->
                  Map.delete key input
          pure (a, input')

compound :: String -> Decoder a -> Decoder a
compound key dec =
  Decoder $ \input ->  do
    let
      nested =
        Map.fromList $
        mapMaybe (\(k, v) -> do
          k' <- stripPrefix (key <> ".") k
          pure (k', v)
        ) $
        Map.toList input

    (a, nested') <- runDecoder dec nested
    pure
      ( a
      , Map.mapKeys (\k -> key <> "." <> k) nested' <> 
        Map.difference input (Map.mapKeys (\k -> key <> "." <> k) nested)
      )

list :: Decoder a -> Decoder [a]
list dec =
  Decoder $ \input -> do
    (n, input') <-
      case Map.lookup "length" input of
        Nothing ->
          Left $ KeyNotFound "length"
        Just (value :| values) -> 
          case readMaybe @Int value of
            Nothing ->
              Left $ DecodeFailure "length" value
            Just n -> do
              let
                input' =
                  case nonEmpty values of
                    Nothing ->
                      Map.delete "length" input
                    Just values' ->
                      Map.insert "length" values' input
              pure (n, input')

    let 
      nested = 
        Map.fromList $
        mapMaybe (\(k, v) -> do
          k' <- stripPrefix "values[]." k
          pure (k', v)) $
        Map.toList input'

      go 0 i =
        pure ([], i)
      go x i = do
        (a, i') <- runDecoder dec i
        (as, i'') <- go (x - 1) i'
        pure (a : as, i'')
        
    (as, nested') <- go n nested
  
    pure
      ( as
      , Map.mapKeys (\k -> "values[]." <> k) nested' <>
        Map.difference input' (Map.mapKeys (\k -> "values[]." <> k) nested)
      )

pair :: Decoder a -> Decoder b -> Decoder (a, b)
pair a b = (,) <$> a <*> b
