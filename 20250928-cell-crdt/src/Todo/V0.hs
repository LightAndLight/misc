{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text
import qualified Data.Text.Lazy.Builder as Text.Builder
import Data.Word (Word64)

data HList (f :: Type -> Type) :: [Type] -> Type where
  HNil :: HList f '[]
  HCons :: f x -> HList f xs -> HList f (x ': xs)

type family Append (xs :: [Type]) (ys :: [Type]) :: [Type] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

happend :: HList f xs -> HList f ys -> HList f (xs `Append` ys)
happend HNil ys = ys
happend (HCons x xs) ys = HCons x (happend xs ys)

data Desc (ctx :: [Type]) (params :: [Type]) a where
  Define ::
    { name :: !Text
    , description :: !Text
    , context :: !(HList (Const Text) ctx)
    , params :: !(HList (Const Text) params)
    , body :: !(Desc ctx params a)
    } ->
    Desc ctx params a
  Opaque ::
    { name :: !Text
    , description :: !Text
    , context :: !(HList (Const Text) ctx)
    , params :: !(HList (Const Text) params)
    , get :: !(HList Binary.Get ctx -> HList Identity params -> Binary.Get a)
    , put :: !(HList (Op Binary.Put) ctx -> HList Identity params -> a -> Binary.Put)
    } ->
    Desc ctx params a
  Field ::
    Text ->
    Desc ctx params a ->
    Desc ctx (a ': params) b ->
    Desc ctx params (a, b)
  End :: Desc ctx params ()
  Iso :: (a -> b, b -> a) -> Desc ctx params a -> Desc ctx params b

descGet :: Desc ctx params a -> HList Binary.Get ctx -> HList Identity params -> Binary.Get a
descGet Define{body} ctx params = descGet body ctx params
descGet Opaque{get} ctx params = get ctx params
descGet (Field _name value rest) ctx params = do
  a <- descGet value ctx params
  b <- descGet rest ctx (HCons (pure a) params)
  pure (a, b)
descGet End _ctx _params = Binary.get @()
descGet (Iso (to, _from) rest) ctx params = to <$> descGet rest ctx params

descPut ::
  Desc ctx params a -> HList (Op Binary.Put) ctx -> HList Identity params -> a -> Binary.Put
descPut Define{body} ctx params a = descPut body ctx params a
descPut Opaque{put} ctx params a = put ctx params a
descPut (Field _name value rest) ctx params (a, b) = do
  descPut value ctx params a
  descPut rest ctx (HCons (pure a) params) b
descPut End _ctx _params () = Binary.put @() ()
descPut (Iso (_to, from) rest) ctx params a = descPut rest ctx params (from a)

asList :: HList (Const a) xs -> [a]
asList HNil = []
asList (HCons (Const x) rest) = x : asList rest

renderNames :: HList (Const Text) xs -> Text.Builder
renderNames = Text.Builder.fromText . Text.unwords . asList

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

documentedDefinition :: MonadState (Map Text Documented) m => Desc ctx params a -> m DocumentedType
documentedDefinition Define{name, description, context, params, body} = do
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
            , dParams = asList $ happend context params
            , dBody = body'
            }
      modify $ Map.insert name doc
  pure $ DocumentedName name
documentedDefinition Opaque{name, description, context, params} = do
  mDesc <- gets $ Map.lookup name
  case mDesc of
    Just{} -> pure ()
    Nothing -> do
      let
        doc =
          DocumentedOpaque
            { dName = name
            , dParams = asList $ happend context params
            , dDefinition = description
            }
      modify $ Map.insert name doc
  pure $ DocumentedName name
documentedDefinition Field{} = error "Field cannot be documented as a definition"
documentedDefinition End = error "End cannot be documented as a definition"
documentedDefinition Iso{} = error "Iso cannot be documented as a definition"

documentedType :: MonadState (Map Text Documented) m => Desc ctx params a -> m DocumentedType
documentedType desc@Field{} = DocumentedStruct <$> documentedStructType desc
documentedType desc@End = DocumentedStruct <$> documentedStructType desc
documentedType (Iso (_to, _from) rest) = documentedType rest
documentedType desc@Define{} = documentedDefinition desc
documentedType desc@Opaque{} = documentedDefinition desc

documentedStructType ::
  MonadState (Map Text Documented) m => Desc ctx params a -> m [(Text, DocumentedType)]
documentedStructType (Field name value rest) = (:) . (,) name <$> documentedType value <*> documentedStructType rest
documentedStructType End = pure []
documentedStructType Iso{} = error "Iso cannot be documented as a struct type"
documentedStructType Define{} = error "Define cannot be documented as a struct type"
documentedStructType Opaque{} = error "Opaque cannot be documented as a struct type"

{-
type Word64 is "An 8 byte natural number (big endian)"
-}
word64 :: Desc '[] '[] Word64
word64 =
  Opaque
    { name = fromString "Word64"
    , context = HNil
    , params = HNil
    , description = fromString "An 8 byte natural number (big endian)"
    , get = \HNil HNil -> Binary.get @Word64
    , put = \HNil HNil -> Binary.put @Word64
    }

type List = []

vector :: Desc '[a] '[Word64] (List a)
vector =
  Opaque
    { name = fromString "Vector"
    , context = HCons (Const $ fromString "a") HNil
    , params = HCons (Const $ fromString "len") HNil
    , description = fromString "`len` consecutive `a`s"
    , get = \(HCons get_a HNil) (HCons (Identity len) HNil) -> do
        replicateM (fromIntegral len) get_a
    , put = \(HCons (Op put_a) HNil) (HCons (Identity _len) HNil) xs -> do
        traverse_ put_a xs
    }

extend :: Desc ctx a b -> Desc (x ': ctx) a b
extend End = End
extend Define{..} =
  Define
    { context = HCons (Const $ fromString "<unnamed>") context
    , body = extend body
    , ..
    }
extend Opaque{..} =
  Opaque
    { context = HCons (Const $ fromString "<unnamed>") context
    , get = \(HCons _ ctx) -> get ctx
    , put = \(HCons _ ctx) -> put ctx
    , ..
    }
extend (Field fieldName fieldDesc rest) =
  Field fieldName (extend fieldDesc) (extend rest)
extend (Iso (from, to) rest) =
  Iso (from, to) (extend rest)

iso :: (a -> b, b -> a) -> Desc ctx params a -> Desc ctx params b
iso = Iso

field ::
  -- | Field name
  Text ->
  -- | Field description
  Desc ctx params a ->
  Desc ctx (a ': params) b ->
  Desc ctx params (a, b)
field = Field

end :: Desc ctx a ()
end = End

lengthWord64 :: [a] -> Word64
lengthWord64 = foldr (\_x -> (+) 1) 0

{-
type List a = {
  len : Word64,
  items : Vector len a
}
-}
list :: Desc '[a] '[] (List a)
list =
  Define
    { name = fromString "List"
    , description = fromString ""
    , context = HCons (Const $ fromString "a") HNil
    , params = HNil
    , body =
        iso (to, from) $
          field (fromString "len") (extend word64) $
            field (fromString "items") vector $
              extend end
    }
  where
    to (_len, (items, ())) = items
    from items = (lengthWord64 items, (items, ()))

{-
# The `List` is sorted in ascending order.
type Set a = List a
-}
set :: Ord a => Desc '[a] '[] (Set a)
set =
  Define
    { name = fromString "Set"
    , description = fromString "The `List` is sorted in ascending order."
    , context = HCons (Const $ fromString "a") HNil
    , params = HNil
    , body = iso (Set.fromAscList, Set.toAscList) list
    }
