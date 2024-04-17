{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
module Html.Class where

import Data.Kind (Type)
import GHC.IsList (IsList (..))
import Data.String (IsString, fromString)
import qualified Data.Text
import Data.Text (Text)
import Maybe (HasMaybe)

class
  ( Monoid (l (Attrs l)), IsList (l (Attrs l)), Item (l (Attrs l)) ~ l (Attr l)
  , Monoid (l (Children l)), IsList (l (Children l)), Item (l (Children l)) ~ l (Node l)
  , Monoid (l Text), IsString (l Text)
  , HasMaybe l
  ) => Html (l :: Type -> Type) where
  type Node l = (r :: Type) | r -> l
  
  text, preEscapedText :: l Text -> l (Node l)
  
  type Attr l = (r :: Type) | r -> l
  attr :: l Text -> l Text -> l (Attr l)
  maybeAttr :: l Text -> l (Maybe Text) -> l (Attr l)

  type Attrs l = (r :: Type) | r -> l
  type Children l = (r :: Type) | r -> l
  el :: l Text -> l (Attrs l) -> l (Children l) -> l (Node l)

fromText :: IsString s => Data.Text.Text -> s
fromText = fromString . Data.Text.unpack
