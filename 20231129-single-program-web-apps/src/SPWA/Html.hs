{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module SPWA.Html (Html (..), setId) where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Json
import Data.Text (Text)
import SPWA.DomEvent (DomEvent (..))
import SPWA.Reactive (Reactive)
import SPWA.Send (Send (..))

data Html
  = Html [Html]
  | Node String [(String, String)] [Html]
  | Void String [(String, String)]
  | WithScript Html String
  | Text String
  | ReactiveText (Reactive String)
  | ReactiveHtml (Reactive Html)
  | OnEvent Html (DomEvent, IO ())

setId :: String -> Html -> Html
setId i (Node name attrs children) = Node name (("id", i) : attrs) children
setId _ a = a

instance Send Html where
  type SendTy Html = Html
  toSendTy = id
  fromSendTy = id

instance ToJSON Html where
  toJSON (Node tag attrs children) =
    Json.object
      [ "tag" .= ("Node" :: Text)
      , "args" .= [toJSON tag, toJSON (fmap toSendTy attrs), toJSON children]
      ]
  toJSON (Text text) =
    Json.object
      [ "tag" .= ("Text" :: Text)
      , "args" .= [text]
      ]

instance FromJSON Html where
  parseJSON = Json.withObject "Html" $ \obj -> do
    tag <- obj .: "tag"
    case tag :: Text of
      "Node" -> do
        args <- obj .: "args"
        Node
          <$> parseJSON (args !! 0)
          <*> (fmap . fmap) fromSendTy (parseJSON (args !! 1))
          <*> parseJSON (args !! 2)
      "Text" -> do
        args <- obj .: "args"
        Text <$> parseJSON (args !! 0)
