{-# LANGUAGE DeriveAnyClass #-}
{-# language LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module App.Api
  ( Api
  , api
  , Routes(..)

  -- * Create comment
  , CreateComment(..)
  , CreatedComment(..)
  , decodeJsonCreatedComment
  ) 
where

import Lib
import Barbies
import Servant hiding (Server)

import Servant.HTML.Blaze (HTML)
import Db (PostSlug, CommentSlug)
import GHC.Generics (Generic)
import Data.Text (Text)
import Type.Reflection (Typeable)
import qualified Js
import Js ((.=))
import Data.Aeson (FromJSON(..), ToJSON (..), (.:))
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity)
import qualified Data.Aeson as Aeson

type Api = NamedRoutes Routes

api :: Proxy Api
api = Proxy

data Routes mode
  = Routes
  { home ::
      mode :- Get '[HTML] Page
  , viewPost ::
      mode :-
        "post" :>
        Capture "slug" PostSlug :>
        Get '[HTML] Page
  , createComment ::
      mode :-
        "post" :>
        Capture "slug" PostSlug :>
        "comment" :>
        ReqBody '[JSON] (CreateComment Server) :>
        Servant.Post '[JSON] (CreatedComment Server)
  } deriving (Generic)

data CreateComment f
  = CreateComment
  { parent :: f (Maybe CommentSlug)
  , content :: f Text
  } deriving (Generic, FunctorB, TraversableB)

deriving instance (forall x. Show x => Show (f x)) => Show (CreateComment f)

data FromRecordField a where
  FromRecordField :: Typeable a => Text -> Js.Expr a -> FromRecordField a

instance FromRecord CreateComment Client where
  fromRecord (CreateComment (C parent) (C content)) =
    C . Js.UnsafeObject $
    bfoldMap (\(FromRecordField k v) -> [(k, Js.SomeExpr v)]) $
    CreateComment
      (FromRecordField "parent" parent)
      (FromRecordField "content" content)

-- TODO: client-side JSON encoding needs to match server-side JSON decoding
instance Server ~ f => FromJSON (CreateComment f) where
  parseJSON =
    Aeson.withObject "CreateComment" $ \obj ->
      btraverse (fmap S) $
      CreateComment
        (obj .: "parent")
        (obj .: "content")

data CreatedComment f
  = CreatedComment
  { slug :: f CommentSlug
  } deriving (Generic, FunctorB, TraversableB)

instance f ~ Server => ToJSON (CreatedComment f) where
  toJSON (CreatedComment slug) =
    Aeson.object .
    bfoldMap (\(Const (k, v)) -> [k Aeson..= v]) $
    CreatedComment (Const ("slug", slug))

-- what about Client?
instance ToRecord CreatedComment Js.Expr where
  toRecord value =
    CreatedComment
      { slug = value `Js.Prj` "slug"
      }

instance f ~ Identity => Js.Debug (CreatedComment f) where
  debug value =
    let CreatedComment slug = toRecord value in
    "CreatedComment{" <>
    ("slug: " <> Js.debug slug) <>
    "}"

decodeJsonCreatedComment ::
  Js.Expr Aeson.Value ->
  Js.Statement r (Js.Expr (Maybe (CreatedComment Identity)))
decodeJsonCreatedComment value = do
  result <- Js.var_
  Js.match
    (value `Js.instanceOf` Js.type_ @(Js.Object Aeson.Value))
    (\case
      Js.PNotInstanceOf ->
        result .= Js.nothing
      Js.PIsInstanceOf value' -> do
        Js.match
          value'.slug
          (\case
            Js.PUndefined ->
              result .= Js.nothing
            Js.PDefined _ ->
              pure ()
          )
        result .= Js.just (Js.UnsafeCast @Aeson.Value @(CreatedComment Identity) value)
    )
  pure result
