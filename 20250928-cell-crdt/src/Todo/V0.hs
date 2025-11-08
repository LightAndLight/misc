{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Todo.V0 where

import Control.Monad (replicateM)
import Control.Monad.State (MonadState, gets, modify)
import qualified Data.Binary as Binary
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)

data HList (f :: a -> Type) :: [a] -> Type where
  HNil :: HList f '[]
  HCons :: f x -> HList f xs -> HList f (x ': xs)

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

happend :: HList f xs -> HList f ys -> HList f (xs `Append` ys)
happend HNil ys = ys
happend (HCons x xs) ys = HCons x (happend xs ys)

data Ty = CodecOf Type | A Type | Arr Ty Ty

infixr 5 `Arr`

type An = A

data TyDecoder :: Ty -> Type where
  DCodecOf :: Binary.Get a -> TyDecoder (CodecOf a)
  DA :: a -> TyDecoder (A a)

data TyEncoder :: Ty -> Type where
  ECodecOf :: (a -> Binary.Put) -> TyEncoder (CodecOf a)
  EA :: a -> TyEncoder (A a)

data Param (a :: Ty)
  = Unused
  | Used !Text

instance IsString (Param a) where
  fromString = Used . fromString

data Params (args :: [Ty]) (ret :: Type) :: Ty -> Type where
  PNil :: Params '[] ret (CodecOf ret)
  PCons :: !Text -> Params xs ret a -> Params (x ': xs) ret (x `Arr` a)

data Desc (ctx :: [Ty]) (a :: Ty) where
  Define ::
    -- | Name
    !Text ->
    -- | Description
    !Text ->
    !(Params params a ty) ->
    !(Desc (params `Append` ctx) ty) ->
    {-
    { name :: !Text
    , description :: !Text
    , params :: !(HList Param params)
    , body :: !(Desc ctx a)
    } ->
    -}
    Desc ctx ty
  Opaque ::
    -- | Name
    !Text ->
    -- | Description
    !Text ->
    !(Params params a ty) ->
    !(HList TyDecoder (params `Append` ctx) -> Binary.Get a) ->
    !(HList TyEncoder (params `Append` ctx) -> a -> Binary.Put) ->
    {-
    { name :: !Text
    , description :: !Text
    -- , params :: !(HList Param params)
    , get :: !(HList TyDecoder ctx -> Binary.Get a)
    , put :: !(HList TyEncoder ctx -> a -> Binary.Put)
    } ->
    -}
    Desc ctx ty
  Field ::
    Text ->
    Desc ctx (CodecOf a) ->
    Desc (An a ': ctx) (CodecOf b) ->
    Desc ctx (CodecOf (a, b))
  End :: Desc ctx (CodecOf ())
  Iso :: (a -> b, b -> a) -> Desc ctx (CodecOf a) -> Desc ctx (CodecOf b)
  Embed :: Desc '[] a -> Desc ctx a

descGet :: Desc params (CodecOf a) -> HList TyDecoder params -> Binary.Get a
descGet (Define _name _desc _params body) params = descGet body params
descGet (Opaque _name _desc PNil get _put) decoders = get decoders
descGet (Field _name value rest) params = do
  a <- descGet value params
  b <- descGet rest (HCons (DA a) params)
  pure (a, b)
descGet End _params = Binary.get @()
descGet (Iso (to, _from) rest) params = to <$> descGet rest params
descGet (Embed rest) _params = descGet rest HNil

descPut :: Desc params (CodecOf a) -> HList TyEncoder params -> a -> Binary.Put
descPut (Define _name _desc _params body) params a = descPut body params a
descPut (Opaque _name _desc PNil _get put) encoders a = put encoders a
descPut (Field _name value rest) params (a, b) = do
  descPut value params a
  descPut rest (HCons (EA a) params) b
descPut End _params () = Binary.put @() ()
descPut (Iso (_to, from) rest) params a = descPut rest params (from a)
descPut (Embed rest) _params a = descPut rest HNil a

asList :: (forall x. f x -> Maybe a) -> HList f xs -> [a]
asList _ HNil = []
asList f (HCons x rest) = maybe id (:) (f x) $ asList f rest

data Documented
  = DocumentedDefine
      { dDescription :: !(Maybe Text)
      , dName :: !Text
      , dParams :: ![Text]
      , dBody :: DocumentedType
      }
  | DocumentedOpaque
      { dName :: !Text
      , dParams :: ![Text]
      , dDefinition :: !Text
      }
  deriving (Show)

data DocumentedType
  = DocumentedName !Text
  | DocumentedStruct ![(Text, DocumentedType)]
  deriving (Show)

documentedDefinition :: MonadState (Map Text Documented) m => Desc params a -> m DocumentedType
documentedDefinition (Embed rest) = documentedDefinition rest
documentedDefinition (Define name description _params body) = do
  body' <- documentedType body
  mDesc <- gets $ Map.lookup name
  case mDesc of
    Just{} -> pure ()
    Nothing -> do
      let
        !doc =
          DocumentedDefine
            { dDescription = if Text.null description then Nothing else Just description
            , dName = name
            , dParams = asList (\case Unused -> Nothing; Used x -> Just x) _
            , dBody = body'
            }
      modify $ Map.insert name doc
  pure $ DocumentedName name
documentedDefinition (Opaque name description _params _get _put) = do
  mDesc <- gets $ Map.lookup name
  case mDesc of
    Just{} -> pure ()
    Nothing -> do
      let
        doc =
          DocumentedOpaque
            { dName = name
            , dParams = asList (\case Unused -> Nothing; Used x -> Just x) _
            , dDefinition = description
            }
      modify $ Map.insert name doc
  pure $ DocumentedName name
documentedDefinition Field{} = error "Field cannot be documented as a definition"
documentedDefinition End = error "End cannot be documented as a definition"
documentedDefinition Iso{} = error "Iso cannot be documented as a definition"

documentedType :: MonadState (Map Text Documented) m => Desc params a -> m DocumentedType
documentedType (Embed rest) = documentedType rest
documentedType desc@Field{} = DocumentedStruct <$> documentedStructType desc
documentedType desc@End = DocumentedStruct <$> documentedStructType desc
documentedType (Iso (_to, _from) rest) = documentedType rest
documentedType desc@Define{} = documentedDefinition desc
documentedType desc@Opaque{} = documentedDefinition desc

documentedStructType ::
  MonadState (Map Text Documented) m => Desc params a -> m [(Text, DocumentedType)]
documentedStructType (Embed rest) = documentedStructType rest
documentedStructType (Field name value rest) = (:) . (,) name <$> documentedType value <*> documentedStructType rest
documentedStructType End = pure []
documentedStructType Iso{} = error "Iso cannot be documented as a struct type"
documentedStructType Define{} = error "Define cannot be documented as a struct type"
documentedStructType Opaque{} = error "Opaque cannot be documented as a struct type"

{-
type Word64 is "An 8 byte natural number (big endian)"
-}
word64 :: Desc ctx (CodecOf Word64)
word64 =
  Opaque
    (fromString "Word64")
    (fromString "An 8 byte natural number (big endian)")
    PNil
    (\_ -> Binary.get @Word64)
    (\_ -> Binary.put @Word64)

type List = []

vector :: Desc ctx (A Word64 `Arr` CodecOf a `Arr` CodecOf (List a))
vector =
  Opaque
    (fromString "Vector")
    (fromString "`len` consecutive `a`s")
    (PCons (fromString "len") (PCons (fromString "a") PNil))
    ( \(HCons (DA len) (HCons (DCodecOf get_a) _ctx)) -> do
        replicateM (fromIntegral len) get_a
    )
    ( \(HCons (EA _len) (HCons (ECodecOf put_a) _ctx)) xs -> do
        traverse_ put_a xs
    )

embed :: Desc '[] a -> Desc params a
embed = Embed

iso :: (a -> b, b -> a) -> Desc params (CodecOf a) -> Desc params (CodecOf b)
iso = Iso

field ::
  -- | Field name
  Text ->
  -- | Field description
  Desc ctx (CodecOf a) ->
  Desc (An a ': ctx) (CodecOf b) ->
  Desc ctx (CodecOf (a, b))
field = Field

end :: Desc params (CodecOf ())
end = End

lengthWord64 :: [a] -> Word64
lengthWord64 = foldr (\_x -> (+) 1) 0

{-
type List a = {
  len : Word64,
  items : Vector len a
}
-}
list :: Desc ctx (CodecOf a `Arr` CodecOf (List a))
list =
  Define
    (fromString "List")
    (fromString "")
    (PCons (fromString "a") PNil)
    ( iso (to, from) $
        field (fromString "len") word64 $
          field (fromString "items") (vector .@ var z .@ var (s z)) $
            end
    )
  where
    to (_len, (items, ())) = items
    from items = (lengthWord64 items, (items, ()))

{-
# The `List` is sorted in ascending order.
type Set a = List a
-}
set :: Ord a => Desc ctx (CodecOf a `Arr` CodecOf (Set a))
set =
  Define
    (fromString "Set")
    (fromString "The `List` is sorted in ascending order.")
    (PCons (fromString "a") PNil)
    (iso (Set.fromAscList, Set.toAscList) list)
