{-# language QualifiedDo #-}
{-# language OverloadedLists #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
module Lib where

import Text.Blaze.Html5 (ToValue, docTypeHtml)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Servant.API hiding (Optional)
import GHC.TypeLits (KnownSymbol, symbolVal, Nat)
import qualified Js
import Data.Text (Text)
import Data.String (IsString (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity (Identity(..))
import Barbies (TraversableB, bsequence', bmap)
import GHC.Records (HasField, getField)
import qualified Html.Class as Html
import Data.Coerce (Coercible)
import qualified Data.Coerce
import Text.Blaze (ToMarkup(..))
import Data.Foldable (for_, fold)
import qualified Text.Blaze.Internal as Blaze
import qualified Data.ByteString.Lazy as Lazy.ByteString
import qualified Data.ByteString.Builder as Builder
import Html.Class (Html, text, preEscapedText, attr, el)
import GHC.IsList (IsList (..))
import Data.Void (Void)
import Id (MonadFresh)
import Data.ByteString.Builder (Builder)
import Data.Text.Encoding (encodeUtf8Builder, encodeUtf8BuilderEscaped)
import Data.ByteString.Builder.Prim (BoundedPrim)
import Data.Word (Word8)
import qualified Data.ByteString.Builder.Prim as BuilderPrim
import Data.Char (ord)
import Maybe (HasMaybe (..))

data Page where
  Page :: a -> (a -> Server (Html.Children Server)) -> Page

instance ToMarkup Page where
  toMarkup (Page a f) =
    docTypeHtml $
      for_ (f a).unS $ \c ->
        for_ (Lazy.ByteString.toChunks $ Builder.toLazyByteString c) $ \chunk ->
          Blaze.Content (Blaze.PreEscaped $ Blaze.ByteString chunk) ()

class UrlEncode expr where
  urlEncode :: ToHttpApiData a => expr a -> expr Text

class AppendStr expr where
  appendStr :: expr Text -> expr Text -> expr Text

data MultitierLinks (l :: Type -> Type)

instance GenericMode (MultitierLinks l) where
  type MultitierLinks l :- api = MkMultitierLink api l

class HasMultitierLink (api :: Type) (l :: Type -> Type) where
  type MkMultitierLink (api :: Type) (l :: Type -> Type) :: Type
  toMultitierLink :: Proxy api -> Proxy l -> l Text -> MkMultitierLink api l

instance (HasMultitierLink a l, HasMultitierLink b l) => HasMultitierLink (a :<|> b) l where
  type MkMultitierLink (a :<|> b) l = MkMultitierLink a l :<|> MkMultitierLink b l
  toMultitierLink _ pl link =
    toMultitierLink (Proxy :: Proxy a) pl link :<|>
    toMultitierLink (Proxy :: Proxy b) pl link

instance
  (ToHttpApiData a, AppendStr l, UrlEncode l, IsString (l Text), HasMultitierLink rest l) =>
  HasMultitierLink (Capture' mods name a :> rest) l
  where
  
  type MkMultitierLink (Capture' mods name a :> rest) l = l a -> MkMultitierLink rest l

  toMultitierLink _ pl link val =
    toMultitierLink (Proxy :: Proxy rest) pl (appendStr (appendStr link "/") (urlEncode val))

instance
  HasMultitierLink rest l =>
  HasMultitierLink (ReqBody' (mods :: [Type]) (contentTypes :: [Type]) (a :: Type) :> rest) l
  where
  
  type MkMultitierLink (ReqBody' mods contentTypes a :> rest) l = MkMultitierLink rest l

  toMultitierLink _ =
    toMultitierLink (Proxy :: Proxy rest)

instance (KnownSymbol segment, AppendStr l, IsString (l Text), HasMultitierLink rest l) => HasMultitierLink (segment :> rest) l where
  type MkMultitierLink (segment :> rest) l = MkMultitierLink rest l

  toMultitierLink _ pl link =
    toMultitierLink (Proxy :: Proxy rest) pl (appendStr (appendStr link "/") (fromString $ symbolVal (Proxy :: Proxy segment)))

instance HasMultitierLink (Verb (method :: k) (status :: Nat) (contentTypes :: [Type]) (a :: Type)) l where
  type MkMultitierLink (Verb method status contentTypes a) l = l Text
  toMultitierLink _ _ link = link

allMultitierLinks ::
  forall l routes.
  ( HasMultitierLink (ToServantApi routes) l
  , GenericServant routes (MultitierLinks l)
  , ToServant routes (MultitierLinks l) ~ MkMultitierLink (ToServantApi routes) l
  , IsString (l Text)
  ) =>
  routes (MultitierLinks l)
allMultitierLinks = fromServant (toMultitierLink (Proxy :: Proxy (ToServantApi routes)) (Proxy :: Proxy l) "")

newtype Server a = S a
  deriving (Eq, Show, IsString, FromHttpApiData, FromJSON, ToJSON, ToValue, Semigroup, Monoid)
  deriving (Functor, Applicative) via Identity

unS :: Server a -> a
unS (S a) = a

instance IsList (Server [a]) where
  type Item (Server [a]) = Server a
  fromList = S . fmap (.unS)
  toList = error "toList not implemented for Server"

instance HasField "unS" (Server a) a where
  getField (S a) = a

instance UrlEncode Server where
  urlEncode (S a) = S (toUrlPiece a)

instance AppendStr Server where
  appendStr (S a) (S b) = S (a <> b)

newtype Client a = C (Js.Expr a)

instance IsList (Client (Js.Array a)) where
  type Item (Client (Js.Array a)) = Client a
  fromList = C . fromList . fmap unC
  toList = error "toList not defined for Client"

unC :: Client a -> Js.Expr a
unC (C a) = a

instance HasField "unC" (Client a) (Js.Expr a) where
  getField (C a) = a

instance Semigroup (Js.Expr a) => Semigroup (Client a) where
 C a <> C b = C (a <> b)

instance Monoid (Js.Expr a) => Monoid (Client a) where
  mempty = C mempty
  
deriving instance a ~ Text => IsString (Client a)

instance UrlEncode Client where
  urlEncode (C a) = C (Js.encodeURIComponent a)

instance AppendStr Client where
  appendStr (C a) (C b) = C (a.concat b)

class FromRecord (shape :: (Type -> Type) -> Type) (l :: Type -> Type) where
  fromRecord :: shape l -> l (shape Identity)

instance TraversableB shape => FromRecord shape Server where
  fromRecord = bsequence'

class ToRecord (shape :: (Type -> Type) -> Type) (l :: Type -> Type) where
  toRecord :: l (shape Identity) -> shape l

instance TraversableB shape => ToRecord shape Server where
  toRecord (S a) = bmap (\(Identity x) -> S x) a

class HasCoerce l where
  coerce :: Coercible a b => l a -> l b

instance HasCoerce Server where
  coerce (S a) = S (Data.Coerce.coerce a)

instance HasCoerce Js.Expr where
  coerce :: forall a b. Coercible a b => Js.Expr a -> Js.Expr b
  coerce =
    -- Js.cast @a @b
    Js.UnsafeCast @a @b

instance HasCoerce Client where
  coerce (C a) = C (Js.UnsafeCast a)

quoted :: Data.Text.Text -> Builder
quoted a = Builder.charUtf8 '"' <> encodeUtf8BuilderEscaped escaper a <> Builder.charUtf8 '"'
  where
    escaper :: BoundedPrim Word8
    escaper =
      BuilderPrim.condB (== ord8 '\\') (fixed2 ('\\', '\\')) $
      BuilderPrim.condB (== ord8 '"') (fixed2 ('\\', '"')) $
      BuilderPrim.liftFixedToBounded BuilderPrim.word8
    
    {-# inline ord8 #-}
    ord8 :: Char -> Word8
    ord8 = fromIntegral . ord 

    {-# INLINE fixed2 #-}
    fixed2 :: (Char, Char) -> BoundedPrim Word8
    fixed2 x =
      BuilderPrim.liftFixedToBounded $
      const x BuilderPrim.>$< BuilderPrim.char7 BuilderPrim.>*< BuilderPrim.char7

instance HasMaybe Server where
  nothing = S Nothing
  just (S a) = S (Just a)
  maybe (S n) j (S ma) = S (Prelude.maybe n (unS . j . S) ma)

instance Html Server where
  type Node Server = Builder
  
  text (S t)=
    -- TODO: escape
    S $ encodeUtf8Builder t
  
  preEscapedText (S t) =
    S $ encodeUtf8Builder t
  
  type Attr Server = Builder
  attr (S name) (S value) =
    S $ encodeUtf8Builder name <> "=" <> quoted value
  maybeAttr name (S mValue) =
    Prelude.maybe (S mempty) (attr name . S) mValue

  type Attrs Server = [Html.Attr Server]
  type Children Server  = [Html.Node Server]
  el (S name) (S attrs) (S children) =
    let name' = encodeUtf8Builder name in
    S $
    "\n<" <> name' <> foldMap (" " <>) attrs <> ">" <>
    fold children <>
    "</" <> name' <> ">"

js :: MonadFresh m => Js.Statement Void a -> m (Server (Html.Node Server))
js st = do
  jsLines <- Js.renderStatement st
  pure $ el "script" [] [preEscapedText . S $ foldMap (<> "\n") jsLines]
