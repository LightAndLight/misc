{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Todo.V0 where

import Control.Monad.State.Class (MonadState, gets, modify)
import qualified Data.Binary as Binary
import Data.Foldable (traverse_)
import Data.Functor.Const (Const (..), getConst)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import GHC.Stack (HasCallStack)

data HList (f :: a -> Type) :: [a] -> Type where
  HNil :: HList f '[]
  HCons :: f x -> HList f xs -> HList f (x ': xs)

data Ty = Codec Type | A Type | Arr Ty Ty

type An = A

infixr 5 `Arr`

data Index :: [Ty] -> Ty -> Type where
  Z :: Index (a ': xs) a
  S :: Index xs b -> Index (a ': xs) b

index :: HList f xs -> Index xs a -> f a
index (HCons x _) Z = x
index (HCons _ xs) (S ix) = index xs ix
index HNil _ = undefined

type family BGet (a :: Ty) :: Type where
  BGet (Arr a b) = BGet a -> BGet b
  BGet (Codec a) = Binary.Get a
  BGet (An a) = a

newtype A_BGet a = A_BGet {fromA_BGet :: BGet a}

type family BPut (a :: Ty) :: Type where
  BPut (Arr a b) = BPut a -> BPut b
  BPut (Codec a) = a -> Binary.Put
  BPut (An a) = a

newtype A_BPut a = A_BPut {fromA_BPut :: BPut a}

data B (ctx :: [Ty]) :: Ty -> Type where
  Named ::
    -- | Name
    !Text ->
    -- | Description
    !Text ->
    B ctx a ->
    B ctx a
  Prim ::
    -- | Description
    !Text ->
    (HList A_BGet ctx -> Binary.Get a) ->
    (HList A_BPut ctx -> a -> Binary.Put) ->
    B ctx (Codec a)
  Var :: Index ctx a -> B ctx a
  Abs :: !Text -> B (a ': ctx) b -> B ctx (a `Arr` b)
  App :: B ctx (a `Arr` b) -> B ctx a -> B ctx b
  Field ::
    -- | Field name
    !Text ->
    B ctx (Codec a) ->
    B (An a ': ctx) (Codec b) ->
    B ctx (Codec (a, b))
  Unit :: B ctx (Codec ())
  Iso :: (a -> b) -> (b -> a) -> B ctx (Codec a) -> B ctx (Codec b)

(.@) :: B ctx (a `Arr` b) -> B ctx a -> B ctx b
(.@) = App

infixl 1 .@

bget :: B ctx a -> HList A_BGet ctx -> BGet a
bget (Var ix) ctx = fromA_BGet $ index ctx ix
bget (Named _name _desc body) ctx = bget body ctx
bget (Prim _desc get _put) ctx = get ctx
bget (Abs _name body) ctx = \d -> bget body (A_BGet d `HCons` ctx)
bget (App f x) ctx =
  bget f ctx (bget x ctx)
bget (Field _name value rest) ctx = do
  a <- bget value ctx
  b <- bget rest (A_BGet a `HCons` ctx)
  pure (a, b)
bget Unit _ctx = Binary.get
bget (Iso to _from rest) ctx = to <$> bget rest ctx

bput :: B ctx a -> HList A_BPut ctx -> BPut a
bput (Var ix) ctx = fromA_BPut $ index ctx ix
bput (Named _name _desc body) ctx = bput body ctx
bput (Prim _ _get put) ctx = put ctx
bput (Abs _name body) ctx = \d -> bput body (A_BPut d `HCons` ctx)
bput (App f x) ctx =
  bput f ctx (bput x ctx)
bput (Field _name value rest) ctx = \(a, b) -> do
  bput value ctx a
  bput rest (A_BPut a `HCons` ctx) b
bput Unit _ctx = Binary.put
bput (Iso _to from rest) ctx = bput rest ctx . from

type List = []

word64 :: B ctx (Codec Word64)
word64 =
  Named (fromString "Word64") (fromString "") $
    Prim (fromString "an 8 byte natural number (big endian)") get put
  where
    get :: HList A_BGet ctx -> Binary.Get Word64
    get _ctx = Binary.get

    put :: HList A_BPut ctx -> Word64 -> Binary.Put
    put _ctx = Binary.put

lengthWord64 :: [a] -> Word64
lengthWord64 = foldr (\_x -> (+) 1) 0

replicateMWord64 :: Applicative m => Word64 -> m a -> m [a]
replicateMWord64 0 _ = pure []
replicateMWord64 n ma = (:) <$> ma <*> replicateMWord64 (n - 1) ma

vector :: B ctx (A Word64 `Arr` Codec a `Arr` Codec (List a))
vector =
  Named (fromString "Vector") (fromString "") $
    Abs (fromString "len") $
      Abs (fromString "a") $
        Prim (fromString "`len` consecutive `a`s") get put
  where
    get :: HList A_BGet (Codec a : A Word64 : ctx) -> Binary.Get (List a)
    get (HCons (A_BGet get_a) (HCons (A_BGet len) _ctx)) = replicateMWord64 len get_a

    put :: HList A_BPut (Codec a : A Word64 : ctx) -> List a -> Binary.Put
    put (HCons (A_BPut put_a) (HCons _len _ctx)) = traverse_ put_a

list :: B ctx (Codec a `Arr` Codec (List a))
list =
  Named (fromString "List") (fromString "") $
    Abs (fromString "a") $
      Iso (\(_len, (xs, ())) -> xs) (\xs -> (lengthWord64 xs, (xs, ()))) $
        Field (fromString "len") word64 $
          Field (fromString "items") (vector .@ Var Z .@ Var (S Z)) $
            Unit

set :: Ord a => B ctx (Codec a `Arr` Codec (Set a))
set =
  Named (fromString "Set") (fromString "The `List` is sorted in ascending order.") $
    Abs (fromString "a") $
      Iso Set.fromAscList Set.toAscList (list .@ Var Z)

data Documented
  = Documented
  { dDescription :: !(Maybe Text)
  , dName :: !Text
  , dParams :: ![Text]
  , dBody :: DocumentedBody
  }
  deriving (Show)

data DocumentedBody
  = DocumentedPrim Text
  | DocumentedType DocumentedType
  deriving (Show)

renderDocumented :: Documented -> String
renderDocumented (Documented mDesc name params body) =
  maybe "" (("# " ++) . Text.unpack) mDesc
    ++ "type "
    ++ Text.unpack name
    ++ (if null params then "" else foldMap ((" " ++) . Text.unpack) params)
    ++ renderDocumentedBody body

renderDocumentedBody :: DocumentedBody -> String
renderDocumentedBody (DocumentedPrim desc) = " is \"" ++ Text.unpack desc ++ "\""
renderDocumentedBody (DocumentedType ty) = " = " ++ renderDocumentedType ty

renderDocumentedType :: DocumentedType -> String
renderDocumentedType (DocumentedName n) = Text.unpack n
renderDocumentedType (DocumentedApp f x) = renderDocumentedType f ++ " " ++ renderDocumentedType x
renderDocumentedType (DocumentedStruct fields) =
  "{ "
    ++ intercalate ", " (fmap (\(name, ty) -> Text.unpack name ++ " : " ++ renderDocumentedType ty) fields)
    ++ " }"

data DocumentedType
  = DocumentedName !Text
  | DocumentedStruct ![(Text, DocumentedType)]
  | DocumentedApp DocumentedType DocumentedType
  deriving (Show)

documented ::
  MonadState (Map Text Documented) m => B ctx a -> HList (Const Text) ctx -> m DocumentedType
documented (Named name desc body) ctx = do
  mDef <- gets $ Map.lookup name
  case mDef of
    Just{} -> pure ()
    Nothing -> do
      (params, body') <- documentedBody body ctx
      let doc = Documented (if Text.null desc then Nothing else Just desc) name params body'
      modify $ Map.insert name doc
  pure $ DocumentedName name
documented _ _ctx = error "expected Named"

documentedBody ::
  MonadState (Map Text Documented) m =>
  B ctx a ->
  HList (Const Text) ctx ->
  m ([Text], DocumentedBody)
documentedBody (Abs name body) ctx = do
  (params, body') <- documentedBody body (HCons (Const name) ctx)
  pure (name : params, body')
documentedBody (Prim desc _get _put) _ctx = pure ([], DocumentedPrim desc)
documentedBody b ctx = (,) [] . DocumentedType <$> documentedType b ctx

documentedType ::
  HasCallStack =>
  MonadState (Map Text Documented) m =>
  B ctx a ->
  HList (Const Text) ctx ->
  m DocumentedType
documentedType b@Named{} ctx = documented b ctx
documentedType (Var ix) ctx = pure $ DocumentedName (getConst $ index ctx ix)
documentedType (App f x) ctx = DocumentedApp <$> documentedType f ctx <*> documentedType x ctx
documentedType b@Field{} ctx = DocumentedStruct <$> documentedStruct b ctx
documentedType (Iso _to _from rest) ctx = documentedType rest ctx
documentedType Unit _ctx = error "got Unit"
documentedType Prim{} _ctx = error "got Prim"
documentedType Abs{} _ctx = error "got Abs"

documentedStruct ::
  MonadState (Map Text Documented) m =>
  B ctx a ->
  HList (Const Text) ctx ->
  m [(Text, DocumentedType)]
documentedStruct (Field name value rest) ctx = do
  a <- documentedType value ctx
  fields <- documentedStruct rest (HCons (Const name) ctx)
  pure $ (name, a) : fields
documentedStruct Unit _ctx = pure []
documentedStruct _ _ctx = error "Not Field or Unit"
