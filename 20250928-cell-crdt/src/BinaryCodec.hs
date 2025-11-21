{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module BinaryCodec where

import Control.Monad (unless)
import Control.Monad.State.Class (MonadState, gets, modify)
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary.Get
import qualified Data.Binary.Put as Binary.Put
import Data.Bitraversable (bitraverse)
import qualified Data.ByteString as ByteString
import Data.Coerce (Coercible, coerce)
import Data.Fixed (E3, Fixed (..), Milli, Pico)
import Data.Foldable (traverse_)
import Data.Functor.Const (Const (..), getConst)
import Data.Kind (Type)
import Data.List (find, intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Data.Time.Clock (UTCTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Void (Void, absurd)
import Data.Word (Word16, Word64, Word8)
import GHC.Stack (HasCallStack)
import Generics.Eot (Eot, HasEot, fromEot, toEot)

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

class (Eq a, Binary a) => IsLit (a :: Type) where
  renderLit :: a -> Text

instance IsLit Word64 where
  renderLit = Text.pack . show

type family Append (xs :: [a]) (ys :: [a]) :: [a] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

data Def :: Ty -> Type where
  Def ::
    -- | Name
    !Text ->
    -- | Description
    !Text ->
    (forall ctx. Body ctx a) ->
    Def a

data Body (ctx :: [Ty]) :: Ty -> Type where
  Prim ::
    -- | Description
    !Text ->
    (HList A_BGet ctx -> Binary.Get a) ->
    (HList A_BPut ctx -> a -> Binary.Put) ->
    Body ctx (Codec a)
  Body :: B ctx a -> Body ctx a
  Param :: !Text -> Body (a ': ctx) b -> Body ctx (a `Arr` b)

data B (ctx :: [Ty]) :: Ty -> Type where
  Var :: Index ctx a -> B ctx a
  Ref :: Def a -> B ctx a
  LiteralUtf8 :: !Text -> B ctx (Codec ())
  App :: B ctx (a `Arr` b) -> B ctx a -> B ctx b
  Field ::
    -- | Field name
    !Text ->
    B ctx (Codec a) ->
    B (An a ': ctx) (Codec b) ->
    B ctx (Codec (a, b))
  Unit :: B ctx (Codec ())
  Cast :: (a -> b) -> (b -> Maybe a) -> B ctx (Codec a) -> B ctx (Codec b)
  Choice :: IsLit a => B ctx (An a) -> [(a, B ctx (Codec b))] -> B ctx (Codec b)

(.@) :: B ctx (a `Arr` b) -> B ctx a -> B ctx b
(.@) = App

infixl 1 .@

bgetBody :: Body ctx a -> HList A_BGet ctx -> BGet a
bgetBody (Body a) ctx = bget a ctx
bgetBody (Param _name rest) ctx = \a -> bgetBody rest (A_BGet a `HCons` ctx)
bgetBody (Prim _desc get _put) ctx = get ctx

bget :: B ctx a -> HList A_BGet ctx -> BGet a
bget (Var ix) ctx = fromA_BGet $ index ctx ix
bget (Ref (Def _name _desc body)) ctx = bgetBody body ctx
bget (LiteralUtf8 expected) _ctx = do
  let expectedBytes = Text.Encoding.encodeUtf8 expected
  actualBytes <- Binary.Get.getByteString (ByteString.length expectedBytes)
  unless (expectedBytes == actualBytes) . fail $ "expected UTF8 string: " ++ Text.unpack expected
bget (App f x) ctx =
  bget f ctx (bget x ctx)
bget (Field _name value rest) ctx = do
  a <- bget value ctx
  b <- bget rest (A_BGet a `HCons` ctx)
  pure (a, b)
bget Unit _ctx = Binary.get
bget (Cast to _from rest) ctx = to <$> bget rest ctx
bget (Choice x ys) ctx = do
  let a = bget x ctx
  case find (\(a', _) -> a == a') ys of
    Nothing ->
      error $
        "expected one of: "
          ++ intercalate ", " (fmap (Text.unpack . renderLit . fst) ys)
          ++ " (got "
          ++ Text.unpack (renderLit a)
          ++ ")"
    Just (_a, rest) -> bget rest ctx

bputBody :: Body ctx a -> HList A_BPut ctx -> BPut a
bputBody (Body a) ctx = bput a ctx
bputBody (Param _name rest) ctx = \a -> bputBody rest (A_BPut a `HCons` ctx)
bputBody (Prim _ _get put) ctx = put ctx

bput :: B ctx a -> HList A_BPut ctx -> BPut a
bput (Var ix) ctx = fromA_BPut $ index ctx ix
bput (Ref (Def _name _desc body)) ctx = bputBody body ctx
bput (LiteralUtf8 expected) _ctx = \() -> Binary.Put.putByteString $ Text.Encoding.encodeUtf8 expected
bput (App f x) ctx =
  bput f ctx (bput x ctx)
bput (Field _name value rest) ctx = \(a, b) -> do
  bput value ctx a
  bput rest (A_BPut a `HCons` ctx) b
bput Unit _ctx = Binary.put
bput (Cast _to from rest) ctx =
  \a ->
    case from a of
      Nothing -> error "conversion failed"
      Just b -> bput rest ctx b
bput (Choice x ys) ctx =
  let a = bput x ctx
  in case find (\(a', _) -> a == a') ys of
      Nothing ->
        error $
          "expected one of: "
            ++ intercalate ", " (fmap (Text.unpack . renderLit . fst) ys)
            ++ " (got "
            ++ Text.unpack (renderLit a)
            ++ ")"
      Just (_a, rest) -> bput rest ctx

iso :: (a -> b) -> (b -> a) -> B ctx (Codec a) -> B ctx (Codec b)
iso to from = Cast to (Just . from)

newtype_ :: Coercible a b => B ctx (Codec a) -> B ctx (Codec b)
newtype_ = iso coerce coerce

record ::
  (HasEot record, Eot record ~ Either fields Void) =>
  B ctx (Codec fields) ->
  B ctx (Codec record)
record = Cast (fromEot . Left) (Just . Prelude.either id absurd . toEot)

type List = []

word8 :: B ctx (Codec Word8)
word8 = Ref def
  where
    def =
      Def (fromString "Word8") (fromString "") $
        Prim (fromString "a byte (big endian)") (const Binary.get) (const Binary.put)

word16 :: B ctx (Codec Word16)
word16 = Ref def
  where
    def =
      Def (fromString "Word16") (fromString "") $
        Prim (fromString "a 2 byte natural number (big endian)") (const Binary.get) (const Binary.put)

word64 :: B ctx (Codec Word64)
word64 = Ref def
  where
    def =
      Def (fromString "Word64") (fromString "") $
        Prim (fromString "an 8 byte natural number (big endian)") (const Binary.get) (const Binary.put)

utcTime :: B ctx (Codec UTCTime)
utcTime = Ref def
  where
    def =
      Def (fromString "UTCTime") (fromString "Milliseconds since 1970-01-01T00:00Z") $
        Body $
          Cast
            ( posixSecondsToUTCTime
                . secondsToNominalDiffTime
                . realToFrac @Milli @Pico
                . MkFixed @Type @E3
                . fromIntegral
            )
            ( ( \(MkFixed n) ->
                  if 0 <= n && n <= fromIntegral (maxBound @Word64)
                    then Just $ fromIntegral @Integer @Word64 n
                    else Nothing
              )
                . realToFrac @Pico @Milli
                . nominalDiffTimeToSeconds
                . utcTimeToPOSIXSeconds
            )
            word64

lengthWord64 :: [a] -> Word64
lengthWord64 = foldr (\_x -> (+) 1) 0

replicateMWord64 :: Applicative m => Word64 -> m a -> m [a]
replicateMWord64 0 _ = pure []
replicateMWord64 n ma = (:) <$> ma <*> replicateMWord64 (n - 1) ma

vector :: B ctx (A Word64 `Arr` Codec a `Arr` Codec (List a))
vector = Ref def
  where
    def =
      Def (fromString "Vector") (fromString "") $
        Param (fromString "len") $
          Param (fromString "a") $
            Prim (fromString "`len` consecutive `a`s") get put

    get :: HList A_BGet (Codec a : A Word64 : ctx) -> Binary.Get (List a)
    get (HCons (A_BGet get_a) (HCons (A_BGet len) _ctx)) = replicateMWord64 len get_a

    put :: HList A_BPut (Codec a : A Word64 : ctx) -> List a -> Binary.Put
    put (HCons (A_BPut put_a) (HCons _len _ctx)) = traverse_ put_a

list :: B ctx (Codec a `Arr` Codec (List a))
list = Ref def
  where
    def =
      Def (fromString "List") (fromString "") $
        Param (fromString "a") $
          Body $
            Cast (\(_len, (xs, ())) -> xs) (\xs -> Just (lengthWord64 xs, (xs, ()))) $
              Field (fromString "len") word64 $
                Field (fromString "items") (vector .@ Var Z .@ Var (S Z)) $
                  Unit

set :: Ord a => B ctx (Codec a `Arr` Codec (Set a))
set = Ref def
  where
    def =
      Def (fromString "Set") (fromString "The `List` is sorted in ascending order.") $
        Param (fromString "a") $
          Body $
            Cast Set.fromAscList (Just . Set.toAscList) (list .@ Var Z)

map :: Ord k => B ctx (Codec k `Arr` Codec v `Arr` Codec (Map k v))
map = Ref def
  where
    def =
      Def
        (fromString "Map")
        (fromString "The `List` is sorted in ascending order on the first element of the pair")
        $ Param (fromString "k")
        $ Param (fromString "v")
        $ Body
        $ Cast Map.fromAscList (Just . Map.toAscList)
        $ list .@ (pair .@ Var (S Z) .@ Var Z)

pair :: B ctx (Codec a `Arr` Codec b `Arr` Codec (a, b))
pair = Ref def
  where
    def =
      Def (fromString "Pair") (fromString "")
        $ Param (fromString "a")
        $ Param (fromString "b")
        $ Body
        $ Cast
          (\(a, (b, ())) -> (a, b))
          (\(a, b) -> Just (a, (b, ())))
        $ Field (fromString "fst") (Var (S Z))
        $ Field (fromString "snd") (Var (S Z))
        $ Unit

either :: Def (Codec a `Arr` Codec b `Arr` Codec (Either a b))
either =
  Def (fromString "Either") (fromString "")
    $ Param (fromString "a")
    $ Param (fromString "b")
    $ Body
    $ Cast
      (\(_tag, (rest, ())) -> rest)
      (\x -> case x of Left{} -> Just (0, (x, ())); Right{} -> Just (1, (x, ())))
    $ Field (fromString "tag") word64
    $ Field
      (fromString "value")
      ( Choice
          (Var Z)
          [ (0, Cast Left (\case Left a -> Just a; Right{} -> Nothing) $ Var (S (S Z)))
          , (1, Cast Right (\case Left{} -> Nothing; Right b -> Just b) $ Var (S Z))
          ]
      )
    $ Unit

literalUtf8 :: Text -> B ctx (Codec ())
literalUtf8 = LiteralUtf8

utf8 :: B ctx (Codec Text)
utf8 = Ref def
  where
    def =
      Def (fromString "Utf8") (fromString "")
        $ Body
          . iso
            (\(_len, (bytes, ())) -> Text.Encoding.decodeUtf8 $ ByteString.pack bytes)
            ( \input ->
                let bytes = Text.Encoding.encodeUtf8 input
                in ( fromIntegral @Int @Word64 $ ByteString.length bytes
                   , (ByteString.unpack bytes, ())
                   )
            )
          . Field (fromString "length") word64
          . Field (fromString "bytes") (vector .@ Var Z .@ word8)
        $ Unit

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

data DocumentedType
  = DocumentedName !Text
  | DocumentedString !Text
  | DocumentedStruct ![(Text, DocumentedType)]
  | DocumentedApp DocumentedType DocumentedType
  | DocumentedChoice DocumentedType [(DocumentedLit, DocumentedType)]
  deriving (Show)

newtype DocumentedLit
  = DocumentedLit Text
  deriving (Show)

documentedDef ::
  MonadState (Map Text Documented) m => Def a -> m DocumentedType
documentedDef (Def name desc body) = do
  mDef <- gets $ Map.lookup name
  case mDef of
    Just{} -> pure ()
    Nothing -> do
      (params, body') <- documentedBody body HNil
      let doc = Documented (if Text.null desc then Nothing else Just desc) name params body'
      modify $ Map.insert name doc
  pure $ DocumentedName name

documentedBody ::
  MonadState (Map Text Documented) m =>
  Body ctx a ->
  HList (Const Text) ctx ->
  m ([Text], DocumentedBody)
documentedBody (Body b) ctx = (,) [] . DocumentedType <$> documentedType b ctx
documentedBody (Param name body) ctx = do
  (params, body') <- documentedBody body (HCons (Const name) ctx)
  pure (name : params, body')
documentedBody (Prim desc _get _put) _ctx = pure ([], DocumentedPrim desc)

documentedType ::
  HasCallStack =>
  MonadState (Map Text Documented) m =>
  B ctx a ->
  HList (Const Text) ctx ->
  m DocumentedType
documentedType (Ref d) _ctx = documentedDef d
documentedType (Var ix) ctx = pure $ DocumentedName (getConst $ index ctx ix)
documentedType (LiteralUtf8 expected) _ctx = pure $ DocumentedString expected
documentedType (App f x) ctx = DocumentedApp <$> documentedType f ctx <*> documentedType x ctx
documentedType b@Field{} ctx = DocumentedStruct <$> documentedStruct b ctx
documentedType (Choice x ys) ctx =
  DocumentedChoice
    <$> documentedType x ctx
    <*> traverse (bitraverse documentedLit (`documentedType` ctx)) ys
documentedType (Cast _to _from rest) ctx = documentedType rest ctx
documentedType Unit _ctx = pure $ DocumentedName (fromString "{}")

documentedLit :: (IsLit a, Applicative m) => a -> m DocumentedLit
documentedLit a = pure . DocumentedLit $ renderLit a

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

renderDocumented :: Documented -> String
renderDocumented (Documented mDesc name params body) =
  maybe "" ((++ "\n") . ("# " ++) . Text.unpack) mDesc
    ++ "type "
    ++ Text.unpack name
    ++ (if null params then "" else foldMap ((" " ++) . Text.unpack) params)
    ++ renderDocumentedBody body

renderDocumentedBody :: DocumentedBody -> String
renderDocumentedBody (DocumentedPrim desc) = " is \"" ++ Text.unpack desc ++ "\""
renderDocumentedBody (DocumentedType ty) = " = " ++ renderDocumentedType ty

renderDocumentedType :: DocumentedType -> String
renderDocumentedType (DocumentedName n) = Text.unpack n
renderDocumentedType (DocumentedString s) =
  "\"" ++ concatMap escape (Text.unpack s) ++ "\""
  where
    escape '\n' = "\\n"
    escape c = [c]
renderDocumentedType (DocumentedApp f x) =
  renderDocumentedType f
    ++ " "
    ++ (case x of DocumentedApp{} -> parens; _ -> id) (renderDocumentedType x)
  where
    parens a = "(" ++ a ++ ")"
renderDocumentedType (DocumentedStruct fields) =
  "{ "
    ++ intercalate ", " (fmap (\(name, ty) -> Text.unpack name ++ " : " ++ renderDocumentedType ty) fields)
    ++ " }"
renderDocumentedType (DocumentedChoice x ys) =
  "match "
    ++ renderDocumentedType x
    ++ " { "
    ++ intercalate ", " (fmap (\(l, y) -> renderDocumentedLit l ++ " -> " ++ renderDocumentedType y) ys)
    ++ " }"

renderDocumentedLit :: DocumentedLit -> String
renderDocumentedLit (DocumentedLit l) = Text.unpack l
