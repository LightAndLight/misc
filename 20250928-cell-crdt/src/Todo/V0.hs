{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Todo.V0 where

import Control.Monad (replicateM)
import Control.Monad.State (MonadState, gets, modify)
import qualified Data.Binary as Binary
import Data.Foldable (traverse_)
import Data.Functor.Const (Const (..))
import Data.Functor.Contravariant (Op (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text
import qualified Data.Text.Lazy.Builder as Text.Builder
import Data.Word (Word64)

data HList (f :: a -> Type) :: [a] -> Type where
  HNil :: HList f '[]
  HCons :: f x -> HList f xs -> HList f (x ': xs)

type family Append (xs :: [Type]) (ys :: [Type]) :: [Type] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

happend :: HList f xs -> HList f ys -> HList f (xs `Append` ys)
happend HNil ys = ys
happend (HCons x xs) ys = HCons x (happend xs ys)

data Ty = CodecOf Type | A Type

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

data Desc (params :: [Ty]) (a :: Type) where
  Define ::
    { name :: !Text
    , description :: !Text
    , params :: !(HList Param params)
    , body :: !(Desc params a)
    } ->
    Desc params a
  Opaque ::
    { name :: !Text
    , description :: !Text
    , params :: !(HList Param params)
    , get :: !(HList TyDecoder params -> Binary.Get a)
    , put :: !(HList TyEncoder params -> a -> Binary.Put)
    } ->
    Desc params a
  Field ::
    Text ->
    Desc params a ->
    Desc (An a ': params) b ->
    Desc params (a, b)
  End :: Desc params ()
  Iso :: (a -> b, b -> a) -> Desc params a -> Desc params b

descGet :: Desc params a -> HList TyDecoder params -> Binary.Get a
descGet Define{body} params = descGet body params
descGet Opaque{get} params = get params
descGet (Field _name value rest) params = do
  a <- descGet value params
  b <- descGet rest (HCons (DA a) params)
  pure (a, b)
descGet End _params = Binary.get @()
descGet (Iso (to, _from) rest) params = to <$> descGet rest params

descPut :: Desc params a -> HList TyEncoder params -> a -> Binary.Put
descPut Define{body} params a = descPut body params a
descPut Opaque{put} params a = put params a
descPut (Field _name value rest) params (a, b) = do
  descPut value params a
  descPut rest (HCons (EA a) params) b
descPut End _params () = Binary.put @() ()
descPut (Iso (_to, from) rest) params a = descPut rest params (from a)

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

data DocumentedType
  = DocumentedName !Text
  | DocumentedStruct ![(Text, DocumentedType)]

documentedDefinition :: MonadState (Map Text Documented) m => Desc params a -> m DocumentedType
documentedDefinition Define{name, description, params, body} = do
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
            , dParams = asList (\case Unused -> Nothing; Used x -> Just x) params
            , dBody = body'
            }
      modify $ Map.insert name doc
  pure $ DocumentedName name
documentedDefinition Opaque{name, description, params} = do
  mDesc <- gets $ Map.lookup name
  case mDesc of
    Just{} -> pure ()
    Nothing -> do
      let
        doc =
          DocumentedOpaque
            { dName = name
            , dParams = asList (\case Unused -> Nothing; Used x -> Just x) params
            , dDefinition = description
            }
      modify $ Map.insert name doc
  pure $ DocumentedName name
documentedDefinition Field{} = error "Field cannot be documented as a definition"
documentedDefinition End = error "End cannot be documented as a definition"
documentedDefinition Iso{} = error "Iso cannot be documented as a definition"

documentedType :: MonadState (Map Text Documented) m => Desc params a -> m DocumentedType
documentedType desc@Field{} = DocumentedStruct <$> documentedStructType desc
documentedType desc@End = DocumentedStruct <$> documentedStructType desc
documentedType (Iso (_to, _from) rest) = documentedType rest
documentedType desc@Define{} = documentedDefinition desc
documentedType desc@Opaque{} = documentedDefinition desc

documentedStructType ::
  MonadState (Map Text Documented) m => Desc params a -> m [(Text, DocumentedType)]
documentedStructType (Field name value rest) = (:) . (,) name <$> documentedType value <*> documentedStructType rest
documentedStructType End = pure []
documentedStructType Iso{} = error "Iso cannot be documented as a struct type"
documentedStructType Define{} = error "Define cannot be documented as a struct type"
documentedStructType Opaque{} = error "Opaque cannot be documented as a struct type"

{-
type Word64 is "An 8 byte natural number (big endian)"
-}
word64 :: Desc '[] Word64
word64 =
  Opaque
    { name = fromString "Word64"
    , params = HNil
    , description = fromString "An 8 byte natural number (big endian)"
    , get = \HNil -> Binary.get @Word64
    , put = \HNil -> Binary.put @Word64
    }

type List = []

vector :: Desc '[A Word64, CodecOf a] (List a)
vector =
  Opaque
    { name = fromString "Vector"
    , params = HCons (fromString "len") (HCons (fromString "a") HNil)
    , description = fromString "`len` consecutive `a`s"
    , get = \(HCons (DA len) (HCons (DCodecOf get_a) HNil)) -> do
        replicateM (fromIntegral len) get_a
    , put = \(HCons (EA _len) (HCons (ECodecOf put_a) HNil)) xs -> do
        traverse_ put_a xs
    }

extend :: Desc params a -> Desc (x ': params) a
extend End = End
extend Define{..} =
  Define
    { params = HCons Unused params
    , body = extend body
    , ..
    }
extend Opaque{..} =
  Opaque
    { params = HCons Unused params
    , get = \(HCons _ ctx) -> get ctx
    , put = \(HCons _ ctx) -> put ctx
    , ..
    }
extend (Field fieldName fieldDesc rest) =
  Field fieldName (extend fieldDesc) (_ rest)
extend (Iso (from, to) rest) =
  Iso (from, to) (extend rest)

iso :: (a -> b, b -> a) -> Desc params a -> Desc params b
iso = Iso

field ::
  -- | Field name
  Text ->
  -- | Field description
  Desc params a ->
  Desc (An a ': params) b ->
  Desc params (a, b)
field = Field

end :: Desc params ()
end = End

lengthWord64 :: [a] -> Word64
lengthWord64 = foldr (\_x -> (+) 1) 0

{-
type List a = {
  len : Word64,
  items : Vector len a
}
-}
list :: Desc '[CodecOf a] (List a)
list =
  Define
    { name = fromString "List"
    , description = fromString ""
    , params = HCons (fromString "a") HNil
    , body =
        iso (to, from) $
          field (fromString "len") (_ word64) $
            field (fromString "items") vector $
              _ end
    }
  where
    to (_len, (items, ())) = items
    from items = (lengthWord64 items, (items, ()))

{-
# The `List` is sorted in ascending order.
type Set a = List a
-}
set :: Ord a => Desc '[CodecOf a] (Set a)
set =
  Define
    { name = fromString "Set"
    , description = fromString "The `List` is sorted in ascending order."
    , params = HCons (fromString "a") HNil
    , body = iso (Set.fromAscList, Set.toAscList) list
    }
