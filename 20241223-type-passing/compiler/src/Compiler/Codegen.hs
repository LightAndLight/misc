{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Compiler.Codegen where

import Compiler.Builtins (Builtins (..), foldMapBuiltins)
import qualified Compiler.Check as Check
import Compiler.Core hiding (Type, kindOf, typeOf)
import qualified Compiler.Core as Core
import Compiler.Fresh (MonadFresh, fresh)
import Compiler.Kind (Kind (..))
import Compiler.OrderedSet (OrderedSet)
import qualified Compiler.OrderedSet as OrderedSet
import Compiler.Syntax (Unique (..))
import Data.Foldable (foldl', for_, traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid (Ap (..))
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import Data.Traversable (for)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Prelude hiding (drop, lines)

import Compiler.Codegen.Builder
import Data.Functor.Identity (Identity, runIdentity)

data TypeInfo = TypeInfo
  { size :: Word
  , alignment :: Word
  , copy :: Builder -> Builder -> BlockT Identity ()
  , move :: Builder -> Builder -> BlockT Identity ()
  , drop :: Builder -> BlockT Identity ()
  }

unitTypeInfo :: TypeInfo
unitTypeInfo =
  TypeInfo
    { size = 0
    , alignment = 0
    , copy =
        -- void(*copy)(*unit dest, *unit src)
        \_dest _src -> do
          pure ()
    , move =
        -- void(*copy)(*unit dest, *unit src)
        \_dest _src -> do
          pure ()
    , drop =
        -- void(*drop)(*unit value)
        \_value -> do
          pure ()
    }

boolTypeInfo :: TypeInfo
boolTypeInfo =
  TypeInfo
    { size = 1
    , alignment = 1
    , copy =
        -- void(*copy)(*bool dest, *bool src)
        \dest src -> do
          assign ("*" <> dest) ("*" <> src)
    , move =
        -- void(*copy)(*bool dest, *bool src)
        \dest src -> do
          assign ("*" <> dest) ("*" <> src)
    , drop =
        -- void(*drop)(*bool value)
        \_value -> do
          pure ()
    }

i32TypeInfo :: TypeInfo
i32TypeInfo =
  TypeInfo
    { size = 4
    , alignment = 4
    , copy =
        -- void(*copy)(*int32_t dest, *int32_t src)
        \dest src -> do
          assign ("*" <> dest) ("*" <> src)
    , move =
        -- void(*copy)(*int32_t dest, *int32_t src)
        \dest src -> do
          assign ("*" <> dest) ("*" <> src)
    , drop =
        -- void(*drop)(*int32_t value)
        \_value -> do
          pure ()
    }

i64TypeInfo :: TypeInfo
i64TypeInfo =
  TypeInfo
    { size = 8
    , alignment = 8
    , copy =
        -- void(*copy)(*int64_t dest, *int64_t src)
        \dest src -> do
          assign ("*" <> dest) ("*" <> src)
    , move =
        -- void(*move)(*int64_t dest, *int64_t src)
        \dest src -> do
          assign ("*" <> dest) ("*" <> src)
    , drop =
        -- void(*drop)(*int64_t value)
        \_value -> do
          pure ()
    }

ptrSize, ptrAlignment :: Word
ptrSize = 8
ptrAlignment = 8

fnTypeInfo :: TypeInfo
fnTypeInfo =
  TypeInfo
    { size = ptrSize
    , alignment = ptrAlignment
    , copy =
        -- void(*copy)(*Rc dest, *Rc src)
        \dest src -> do
          assign ("*" <> dest) $ call "Rc_copy" ["*" <> src]
    , move =
        -- void(*move)(*Rc dest, *Rc src)
        \dest src -> do
          assign ("*" <> dest) ("*" <> src)
    , drop =
        -- void(*drop)(*Rc value)
        \value -> do
          statement $ call "Rc_drop" ["*" <> value]
    }

getTypeInfo :: Core.Type -> Maybe TypeInfo
getTypeInfo TVar{} = Nothing
getTypeInfo TUnit = Just unitTypeInfo
getTypeInfo TBool = Just boolTypeInfo
getTypeInfo TI32 = Just i32TypeInfo
getTypeInfo TI64 = Just i64TypeInfo
getTypeInfo TFn{} = Just fnTypeInfo
getTypeInfo ty = error $ "TODO: type info for " ++ show ty

data CDefinition m
  = CFunction
      -- | Return type
      Builder
      -- | Name
      Builder
      -- | Arguments
      [(Builder, Builder)]
      -- | Body
      (BlockT m ())
  | CTypedef
      -- | Type
      Builder
      -- | Alias
      Builder
  | CConst
      -- | Type
      Builder
      -- | Name
      Builder
      -- | Value
      Builder

forHeader :: Monad m => CDefinition m -> ModuleT m ()
forHeader (CFunction ret name args _) = functionSignature ret name args
forHeader (CTypedef ty alias) = typedef ty alias
forHeader (CConst ty name _) = externConst ty name

forImpl :: Monad m => CDefinition m -> ModuleT m ()
forImpl (CFunction ret name args body) = function ret name args body
forImpl CTypedef{} = pure ()
forImpl (CConst ty name val) = const' ty name val

builtins :: Monad m => Builtins (Builder -> CDefinition m)
builtins =
  Builtins
    { trace_i32 = \name ->
        CFunction "Unit" name [("int32_t", "x")] $ do
          statement $ call "printf" [Builder.fromString $ show ("%d\n" :: String), "x"]
          return' "unit"
    , trace_bool = \name ->
        CFunction "Unit" name [("bool", "x")] $ do
          ifThenElse
            "x"
            (statement $ call "printf" [Builder.fromString $ show ("true\n" :: String)])
            (statement $ call "printf" [Builder.fromString $ show ("false\n" :: String)])
          return' "unit"
    , add_i32 = \name ->
        CFunction "int32_t" name [("int32_t", "x"), ("int32_t", "y")] $ do
          return' "x + y"
    }

genBuiltinsHeader :: Monad m => ModuleT m ()
genBuiltinsHeader = do
  section $ do
    line "#ifndef builtins_h"
    line "#define builtins_h"

  includes ["<stdbool.h>", "<stdint.h>"]

  typedef "struct{}" "Unit"
  externConst "Unit" "unit"

  getAp $ foldMapBuiltins (\name fn -> Ap $ forHeader (fn $ ident name)) builtins
  section $ line "#endif"

genBuiltins :: Monad m => ModuleT m ()
genBuiltins = do
  includes ["<stdbool.h>", "<stdint.h>", "<stdio.h>", "\"builtins.h\""]

  const' "Unit" "unit" "{}"

  getAp $ foldMapBuiltins (\name fn -> Ap $ forImpl (fn $ ident name)) builtins

data Env = Env
  { globals :: HashMap Text Core.Type
  , locals :: HashMap Var (TypeOrKind, Ownership, Builder)
  }

defaultEnv :: Env
defaultEnv =
  Env
    { globals = mempty
    , locals = mempty
    }

data Ownership = Owned | Borrowed
  deriving (Show, Eq)

withLocals :: Foldable f => f (Var, TypeOrKind, Builder) -> Env -> Env
withLocals = foldl' (\acc (u, typeOrKind, code) -> acc . withLocal u typeOrKind code) id

borrowing :: HashSet Var -> Env -> Env
borrowing vars e =
  e
    { locals =
        foldl'
          ( \acc var ->
              HashMap.adjust
                (\(ty, _, code) -> (ty, Borrowed, code))
                var
                acc
          )
          (locals e)
          (HashSet.toList vars)
    }

rewritingCode :: HashMap Var Builder -> Env -> Env
rewritingCode rewrites e =
  e
    { locals =
        foldl'
          ( \acc (u, newCode) ->
              HashMap.adjust (\(ty, o, _) -> (ty, o, newCode)) u acc
          )
          (locals e)
          (HashMap.toList rewrites)
    }

withLocal :: Var -> TypeOrKind -> Builder -> Env -> Env
withLocal u typeOrKind varCode e =
  e{locals = HashMap.insert u (typeOrKind, Owned, varCode) (locals e)}

getVar :: Env -> Var -> (TypeOrKind, Ownership, Builder)
getVar e v = fromMaybe undefined . HashMap.lookup v $ locals e

getOwned :: Env -> HashSet Var
getOwned e =
  let ls = locals e
  in HashSet.fromList
      ( mapMaybe
          ( \(v, (_, o, _)) ->
              case o of
                Owned -> Just v
                Borrowed -> Nothing
          )
          (HashMap.toList ls)
      )

typeOf :: Env -> Expr -> TypeOrKind
typeOf e expr =
  let ctx = globals e
  in let ls = locals e
     in Core.typeOf ctx (fmap (\(a, _, _) -> a) ls) expr

kindOf :: Env -> Core.Type -> TypeOrKind
kindOf e ty =
  let ls = locals e
  in Core.kindOf (fmap (\(a, _, _) -> a) ls) ty

genModule :: MonadFresh m => Module -> ModuleT m ()
genModule (Module defs) = do
  includes
    [ "<stdalign.h>"
    , "<stdbool.h>"
    , "<stdint.h>"
    , "\"runtime.h\""
    , "\"builtins.h\""
    ]

  traverse_ (genDefinition defaultEnv{globals = ctx}) defs
  where
    ctx =
      foldl'
        ( \acc def ->
            case def of
              Binding name Nothing ret _ -> HashMap.insert name ret acc
              Binding name (Just args) ret _ -> HashMap.insert name (TFn args ret) acc
        )
        (foldMapBuiltins HashMap.singleton Check.builtins)
        defs

freshName :: MonadFresh m => Builder -> m Builder
freshName s = do
  n <- fresh
  pure $ s <> "_" <> Builder.fromString (show n)

{-| Check whether a type has a dynamic layout, and if so, return the variables on
which the layout depends.
-}
isAbstract :: Core.Type -> Maybe (OrderedSet Var)
isAbstract (TVar v) = Just $ OrderedSet.singleton v
isAbstract (TExists _name _k _rest) = Nothing
isAbstract (TFn _args _ret) = Nothing
isAbstract (TRecord fields) =
  let vars = (foldMap . foldMap) (fromMaybe mempty . isAbstract) fields
  in if OrderedSet.null vars
      then Nothing
      else Just vars
isAbstract TUnit = Nothing
isAbstract TBool = Nothing
isAbstract TI32 = Nothing
isAbstract TI64 = Nothing
isAbstract TArray{} = Nothing

genTypeReturnType :: Core.Type -> Builder
genTypeReturnType ty =
  case isAbstract ty of
    Nothing -> genTypeArgType ty
    Just{} -> "void"

genTypeArgType :: Core.Type -> Builder
genTypeArgType TVar{} = "void*"
genTypeArgType (TExists _name k _rest) =
  tyStruct [(genKind k, "fst"), ("Rc", "snd")]
genTypeArgType TFn{} = "Rc"
genTypeArgType (TRecord fields) =
  tyStruct $ fmap (\(field, ty) -> (genTypeArgType ty, ident field)) (Vector.toList fields)
genTypeArgType TUnit = "Unit"
genTypeArgType TBool = "bool"
genTypeArgType TI32 = "int32_t"
genTypeArgType TI64 = "int64_t"
genTypeArgType TArray{} = "Array"

genTypeOrKindArgType :: TypeOrKind -> Builder
genTypeOrKindArgType (Core.Type ty) = genTypeArgType ty
genTypeOrKindArgType (Kind ki) = genKind ki

genKind :: Kind -> Builder
genKind KType = "const Type*"
genKind KArrow{} = "const TypeCon*"
genKind KUnknown = error "KUnknown"

genTypeVal :: Monad m => Env -> Core.Type -> BlockT m Builder
genTypeVal env (TVar v) = do
  let (_typeOrKind, _ownership, varName) = getVar env v
  pure varName
genTypeVal _env (TExists _name _k _rest) = pure "&Type_exists"
genTypeVal _env TFn{} = pure "&Type_fn"
genTypeVal _env (TRecord _fields) = error "TODO: type dictionary for records"
genTypeVal _env TUnit = pure "&Type_unit"
genTypeVal _env TBool = pure "&Type_bool"
genTypeVal _env TI32 = pure "&Type_i32"
genTypeVal _env TI64 = pure "&Type_i64"
genTypeVal _env TArray{} = pure "&Type_array"

genUnique :: Unique -> Builder
genUnique (U u) = "v_" <> Builder.fromString (show u)

genNamed :: Text -> Builder
genNamed n = "l_" <> ident n

genVarName :: Var -> Builder
genVarName (VNamed n) = genNamed n
genVarName (VUnique u) = genUnique u

genDefinition :: MonadFresh m => Env -> Definition -> ModuleT m ()
genDefinition _env (Binding _name Nothing _ret _body) =
  error "TODO: genDefinition for constant"
genDefinition env (Binding name (Just args) ret body) = do
  mResultArg <-
    for (isAbstract ret) $ \_ -> do
      resultName <- freshName "result"
      pure (genTypeArgType ret, resultName)

  argNames <- for (Vector.toList args) $ \(mArgName, _argTy) -> do
    case mArgName of
      Nothing -> freshName "unused"
      Just v -> pure $ genVarName v

  let
    argsCode =
      maybe [] pure mResultArg
        ++ zipWith
          (\(_, typeOrKind) argName -> (genTypeOrKindArgType typeOrKind, argName))
          (Vector.toList args)
          argNames

  let args' =
        Vector.mapMaybe (\(mVar, typeOrKind) -> fmap (\var -> (var, typeOrKind, genVarName var)) mVar) args

  let env' = withLocals args' env
  function (genTypeReturnType ret) (ident name) argsCode $ do
    resultName <-
      case mResultArg of
        Nothing -> do
          resultName <- freshName "result"
          declare (genTypeArgType ret) resultName
          pure resultName
        Just (_, resultName) ->
          pure resultName

    genExpr env' resultName body

    case mResultArg of
      Nothing -> return' resultName
      Just{} -> pure ()

genMove :: MonadFresh m => Env -> TypeOrKind -> Builder -> Builder -> BlockT m ()
genMove _env Kind{} _dest _src = error "TODO: move types?"
genMove env (Core.Type ty) dest src = do
  let
    (destCode, srcCode) = case isAbstract ty of
      Nothing -> ("&" <> dest, "&" <> src)
      Just{} -> (dest, src)

  case getTypeInfo ty of
    Nothing -> do
      tyCode <- genTypeVal env ty
      statement $ call "Type_move" [tyCode, destCode, srcCode]
    Just typeInfo -> do
      hoistBlockT (pure . runIdentity) $ move typeInfo destCode srcCode

genCopy :: MonadFresh m => Env -> TypeOrKind -> Builder -> Builder -> BlockT m ()
genCopy _env Kind{} _dest _src = error "TODO: copy types?"
genCopy env (Core.Type ty) dest src = do
  let
    (destCode, srcCode) = case isAbstract ty of
      Nothing -> ("&" <> dest, "&" <> src)
      Just{} -> (dest, src)

  case getTypeInfo ty of
    Nothing -> do
      tyCode <- genTypeVal env ty
      statement $ call "Type_copy" [tyCode, destCode, srcCode]
    Just typeInfo -> do
      hoistBlockT (pure . runIdentity) $ copy typeInfo destCode srcCode

genDrop :: MonadFresh m => Env -> TypeOrKind -> Builder -> BlockT m ()
genDrop _env Kind{} _value = error "TODO: drop types?"
genDrop env (Core.Type ty) value = do
  let
    valueCode = case isAbstract ty of
      Nothing -> "&" <> value
      Just{} -> value

  case getTypeInfo ty of
    Nothing -> do
      tyCode <- genTypeVal env ty
      statement $ call "Type_drop" [tyCode, valueCode]
    Just typeInfo -> do
      hoistBlockT (pure . runIdentity) $ drop typeInfo valueCode

genVar :: MonadFresh m => Env -> Builder -> Var -> BlockT m ()
genVar env result v = do
  let (typeOrKind, ownership, varCode) = getVar env v
  case ownership of
    Owned ->
      genMove env typeOrKind result varCode
    Borrowed ->
      genCopy env typeOrKind result varCode

relevantFreeVars :: FreeVars -> HashSet Var
relevantFreeVars =
  HashSet.fromList
    . mapMaybe (\(var, m) -> case m of Zero -> Nothing; _ -> Just var)
    . toListFreeVars

genArgument ::
  MonadFresh m =>
  Env ->
  Builder ->
  Argument ->
  BlockT m ()
genArgument env result (ArgValue expr _ty) =
  genValue env result expr
genArgument env result (ArgType ty _ki) = do
  tyCode <- genTypeVal env ty
  assign result tyCode

genValue :: MonadFresh m => Env -> Builder -> Value -> BlockT m ()
genValue _env _result (Name name) =
  error $ "code gen for name " <> show name
genValue env result (Var v) =
  genVar env result v
genValue env result expr@(Lam args body) = do
  bodyTy <-
    case typeOf env body of
      Core.Type ty ->
        pure ty
      Kind{} ->
        error "closure returned a type instead of a value"

  let
    captures :: [Var]
    captures =
      mapMaybe (\(v, m) -> case m of Zero -> Nothing; _ -> Just v) . toListFreeVars $
        freeVarsValue Just expr

  let
    closureTypeCaptures =
      fmap
        ( \v ->
            let (typeOrKind, _, _) = getVar env v
            in ( genTypeOrKindArgType typeOrKind
               , genVarName v
               )
        )
        captures

  let closureType = tyStruct $ ("FP_Closure_apply", "apply") : closureTypeCaptures
  closureTypeName <- freshName "Closure"
  toplevel $ typedef closureType closureTypeName

  closureCodeName <- freshName "closure_code"
  selfName <- freshName "self"
  mResultArgCode <-
    for (isAbstract bodyTy) $ \_ -> do
      resultName <- freshName "result"
      pure (genTypeArgType bodyTy, resultName)
  closureArgCodes <- do
    argCodes <-
      traverse
        ( \(mVar, typeOrKind) -> do
            argCode <- case mVar of
              Nothing -> freshName "unused"
              Just var -> pure $ genVarName var
            pure (genTypeOrKindArgType typeOrKind, argCode)
        )
        (Vector.toList args)

    let selfArgCode = ("const " <> closureTypeName <> "*", selfName)
    let resultArgCode = maybe [] pure mResultArgCode
    pure $ selfArgCode : resultArgCode ++ argCodes

  let
    rewrites =
      HashMap.fromList $
        zipWith (\u (_, name) -> (u, selfName <> "->" <> name)) captures closureTypeCaptures
    args' =
      mapMaybe (\(mVar, typeOrKind) -> fmap (\var -> (var, typeOrKind, genVarName var)) mVar) $
        Vector.toList args

  let env' = rewritingCode rewrites $ withLocals args' env
  toplevel . function (genTypeReturnType bodyTy) closureCodeName closureArgCodes $ do
    resultName <-
      case mResultArgCode of
        Nothing -> do
          resultName <- freshName "result"
          declare (genTypeArgType bodyTy) resultName
          pure resultName
        Just (_, resultName) ->
          pure resultName

    genExpr env' resultName body

    case mResultArgCode of
      Nothing -> return' resultName
      Just{} -> pure ()

  assign result $ call "Rc_alloc" [call "alignof" [closureTypeName], call "sizeof" [closureTypeName]]
  block $ do
    dataPtrName <- freshName "data"
    define (closureTypeName <> "*") dataPtrName $ call "Rc_data" [result]

    assign (dataPtrName <> "->apply") $ cast "FP_Closure_apply" ("&" <> closureCodeName)
    for_ (zip captures closureTypeCaptures) $ \(u, (_typeCode, name)) ->
      genVar env (dataPtrName <> "->" <> name) u
genValue env result expr@(Record fields) = do
  name <- freshName "record"
  let exprTy = typeOf env $ pure expr
  declare (genTypeOrKindArgType exprTy) name

  traverse_
    (\(fieldName, fieldExpr) -> genValue env (result <> "." <> ident fieldName) fieldExpr)
    fields
genValue env result (Proj record field) = do
  recordName <- freshName "record"
  genValue env recordName record
  assign result $ recordName <> "." <> ident field
genValue env result (Pack ty a b) = do
  a' <- genTypeVal env a

  bName <- freshName "b"
  declare (genTypeArgType ty) bName
  genValue env bName b

  let
    bTy =
      case typeOf env $ pure b of
        Core.Type bTy' -> bTy'
        Kind{} -> undefined

  bTyName <- freshName "type"
  bTy' <- genTypeVal env bTy
  define (genKind KType) bTyName bTy'

  bPtrName <- freshName "snd"
  define "Rc" bPtrName $ call "Rc_alloc" [call "Type_alignment" [bTyName], call "Type_size" [bTyName]]
  statement $ call "Type_move" [bTyName, call "Rc_data" [bPtrName], "&" <> bName]

  assign result $ struct [("fst", a'), ("snd", bPtrName)]
genValue _env result (Bool b) = if b then assign result "true" else assign result "false"
genValue _env result (I32 i) = assign result $ Builder.fromString (show i)
genValue _env result (I64 i) = assign result $ Builder.fromString (show i)
genValue env result (Array itemTy items) = do
  let
    (itemsBorrows, _remainingOwned) =
      borrows
        (getOwned env)
        (Vector.toList $ fmap (relevantFreeVars . freeVarsValue Just) items)

  let itemCount = Vector.length items
  itemTyCode <- genTypeVal env itemTy
  assign result $ call "Array_alloc" [itemTyCode, int itemCount]
  block $ do
    dataName <- freshName "data"
    define "char*" dataName $ call "Rc_data" [result <> ".data"]
    case getTypeInfo itemTy of
      Nothing -> do
        sizeCode <- do
          sizeName <- freshName "size"
          define "size_t" sizeName $ "(" <> itemTyCode <> ")" <> "->size"
          pure sizeName

        for_ (zip [0 :: Int ..] . zip itemsBorrows $ Vector.toList items) $ \(ix, (itemBorrows, item)) -> do
          genValue
            (borrowing itemBorrows env)
            ("(" <> dataName <> " + " <> int ix <> "*" <> sizeCode <> ")")
            item
      Just itemTyInfo -> do
        let sizeStatic = size itemTyInfo
        for_ (zip [0 :: Word ..] . zip itemsBorrows $ Vector.toList items) $ \(ix, (itemBorrows, item)) -> do
          genValue
            (borrowing itemBorrows env)
            ("*" <> cast (genTypeArgType itemTy <> "*") (parens $ dataName <> " + " <> word (ix * sizeStatic)))
            item

borrows ::
  -- | Initial owned variables
  HashSet Var ->
  -- | Sequence of free variables
  [HashSet Var] ->
  ( -- \| Variables that each element of the previous sequence must borrow
    [HashSet Var]
  , -- \| Remaining owned variables
    HashSet Var
  )
borrows ownedVars =
  foldr
    ( \argFreeVars (xs', alreadyOwnedVars) ->
        let
          !argOwnedVars = HashSet.intersection argFreeVars ownedVars
          !alreadyOwnedVars' = HashSet.union argOwnedVars alreadyOwnedVars
        in
          ( alreadyOwnedVars : xs'
          , alreadyOwnedVars'
          )
    )
    ([], HashSet.empty)

genComp :: MonadFresh m => Env -> Comp -> BlockT m [(Core.Var, TypeOrKind, Builder)]
genComp env (Let v ty val) = do
  let valName = genVarName v
  declare (genTypeArgType ty) valName
  genValue env valName val

  pure [(v, Core.Type ty, valName)]
genComp env (Call v ty f xs) = do
  let
    xsVars :: Vector (HashSet Core.Var)
    xsVars =
      fmap
        ( \case
            ArgValue e _ -> relevantFreeVars $ freeVarsValue Just e
            ArgType t _ -> relevantFreeVars $ freeVarsType Just t
        )
        xs

  let ownedVars = getOwned env
  let (xsBorrowedVars, xsOwnedVars) = borrows ownedVars $ Vector.toList xsVars

  let
    fTy =
      case typeOf env $ pure f of
        Core.Type t -> t
        Kind{} -> undefined

  (argTys, retTy) <- do
    case fTy of
      TFn argTys retTy -> pure (argTys, retTy)
      _ -> error $ "function " <> show f <> " has type " <> show fTy

  f' <-
    case f of
      Name fName ->
        pure $ ident fName
      _ -> do
        fName <- freshName "func"
        declare (genTypeArgType fTy) fName
        genValue (borrowing xsOwnedVars env) fName f
        pure fName

  xs' <-
    traverse
      ( \(bs, (x, argTypeOrKind)) -> do
          argName <- freshName "arg"
          let
            xTy =
              case x of
                ArgValue _e t -> Core.Type t
                ArgType _t k -> Kind k
          declare (genTypeOrKindArgType xTy) argName
          genArgument (borrowing bs env) argName x

          case argTypeOrKind of
            Core.Type argTy | Just{} <- isAbstract argTy -> pure $ "&" <> argName
            _ -> pure argName
      )
      (zip xsBorrowedVars . zip (Vector.toList xs) $ snd <$> Vector.toList argTys)

  let vCode = genVarName v
  mResultArg <-
    for (isAbstract retTy) $ \_ -> do
      tyCode <- genTypeVal env ty
      define "void*" vCode $
        call "aligned_alloca" [call "Type_alignment" [tyCode], call "Type_size" [tyCode]]
      pure $ cast (genTypeArgType ty <> "*") vCode

  called <- case f of
    Name{} ->
      pure . call f' $ maybe [] pure mResultArg ++ xs'
    _ -> do
      fDataName <- freshName "f"
      define "Closure*" fDataName $ call "Rc_data" [f']

      applyName <- freshName "apply"
      assign
        (closureApplyFunctionPointerLHS applyName (Vector.toList argTys) retTy)
        ( cast
            (closureApplyFunctionPointerType (Vector.toList argTys) retTy)
            (fDataName <> "->apply")
        )
      pure . call applyName $ fDataName : maybe [] pure mResultArg ++ xs'

  case mResultArg of
    Nothing -> define (genTypeArgType ty) vCode called
    Just{} -> statement called

  case f of
    Name{} -> pure ()
    _ -> genDrop env (Core.Type fTy) f'

  pure [(v, Core.Type ty, maybe vCode ("*" <>) mResultArg)]
  where
    closureApplyFunctionPointerLHS :: Builder -> [(Maybe Var, TypeOrKind)] -> Core.Type -> Builder
    closureApplyFunctionPointerLHS name args ret =
      lvalFunctionPointer
        (genTypeReturnType ret)
        name
        ("Closure*" : fmap (genTypeOrKindArgType . snd) args)

    closureApplyFunctionPointerType :: [(Maybe Var, TypeOrKind)] -> Core.Type -> Builder
    closureApplyFunctionPointerType = closureApplyFunctionPointerLHS mempty
genComp env (LetExists (tyName, tyKind) (exprName, exprTy) val) = do
  valName <- freshName "exists"
  genValue env valName val
  pure
    [ (tyName, Kind tyKind, valName <> ".fst")
    , (exprName, Core.Type exprTy, valName <> ".snd")
    ]

genExpr :: MonadFresh m => Env -> Builder -> Expr -> BlockT m ()
genExpr e result (Expr cs val) = go e cs val
  where
    go :: MonadFresh m => Env -> Vector Comp -> Value -> BlockT m ()
    go env comps value =
      case Vector.uncons comps of
        Nothing ->
          genValue env result value
        Just (comp, comps') -> do
          let rest = Expr comps' value
          let restVars = relevantFreeVars $ freeVarsExpr Just rest

          let ownedVars = getOwned env

          let restOwned = HashSet.intersection ownedVars restVars

          ls <- genComp (borrowing restOwned env) comp
          go (withLocals ls env) comps' value
