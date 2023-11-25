{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.DeepSeq (force)
import Control.Exception (SomeException, evaluate)
import Control.Monad ((<=<))
import Control.Monad.Catch (try)
import Control.Monad.IO.Class (liftIO)
import Data.GADT.Show (GShow (..), defaultGshowsPrec)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Some (Some (..))
import Env (MonadEnviron (..), runEnvironT)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified System.Environment

main :: IO Bool
main = checkParallel $$discover

data MonadEnvironSyntax a where
  GetEnviron :: MonadEnvironSyntax (Map String String)
  SetEnviron :: Map String String -> MonadEnvironSyntax ()
  GetVar :: String -> MonadEnvironSyntax (Maybe String)
  SetVar :: String -> String -> MonadEnvironSyntax ()
  Then :: MonadEnvironSyntax a -> MonadEnvironSyntax b -> MonadEnvironSyntax b

deriving instance Show (MonadEnvironSyntax a)

instance GShow MonadEnvironSyntax where
  gshowsPrec = defaultGshowsPrec

runMonadEnvironSyntax :: (MonadEnviron m) => MonadEnvironSyntax a -> m a
runMonadEnvironSyntax GetEnviron = getEnviron
runMonadEnvironSyntax (SetEnviron e) = setEnviron e
runMonadEnvironSyntax (GetVar k) = getVar k
runMonadEnvironSyntax (SetVar k v) = setVar k v
runMonadEnvironSyntax (Then ma mb) = runMonadEnvironSyntax ma *> runMonadEnvironSyntax mb

genString :: (MonadGen m) => m String
genString = Gen.string (Range.constant 0 500) Gen.unicode

genEnviron :: (MonadGen m) => m (Map String String)
genEnviron =
  Map.fromList <$> Gen.list (Range.constant 0 500) ((,) <$> genString <*> genString)

genMonadEnvironSyntax :: (MonadGen m) => Set String -> m (Some MonadEnvironSyntax)
genMonadEnvironSyntax initialKeys =
  Gen.recursive
    Gen.choice
    [ fmap Some $ GetVar <$> genVar
    , fmap Some $ SetVar <$> genVar <*> genString
    , fmap Some $ pure GetEnviron
    , fmap Some
        $ SetEnviron
        <$> genEnviron
    ]
    [ (\(Some a) (Some b) -> Some (Then a b))
        <$> genMonadEnvironSyntax initialKeys
        <*> genMonadEnvironSyntax initialKeys
    ]
 where
  genVar = Gen.choice $ [genString] <> [Gen.element initialKeys | not (null initialKeys)]

genKeyLinux :: (MonadGen m) => m String
genKeyLinux =
  Gen.string (Range.constant 1 100) (Gen.filterT (`notElem` "\0=") Gen.ascii)

genValueLinux :: (MonadGen m) => m String
genValueLinux =
  Gen.string (Range.constant 1 100) (Gen.filterT (`notElem` "\0") Gen.ascii)

prop_environ_correct :: Property
prop_environ_correct =
  property $ do
    initial <- forAll genEnviron
    Some program <- forAll $ genMonadEnvironSyntax (Map.keysSet initial)
    final <- do
      result <- try . liftIO $ do
        setEnviron initial
        runMonadEnvironSyntax program
        getEnviron
      case result of
        Left (err :: SomeException) -> do
          annotateShow err
          failure
        Right a ->
          pure a
    (final', _) <- runEnvironT (runMonadEnvironSyntax program) initial
    final === final'