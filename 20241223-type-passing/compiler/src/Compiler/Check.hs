{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Compiler.Check where

import Compiler.Builtins (Builtins (..), foldMapBuiltins)
import qualified Compiler.Core as Core
import Compiler.Fresh (FreshT, fresh, runFreshT)
import Compiler.Kind (Kind (..))
import Compiler.Syntax
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT (..), asks, runReaderT)
import Control.Monad.Reader.Class (local)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer.CPS (runWriterT)
import Control.Monad.Writer.Class (tell)
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid (Any (..))
import Data.Text (Text)
import Data.Traversable (for)
import qualified Data.Tuple as Tuple
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Stack (HasCallStack, withFrozenCallStack)

builtins :: Builtins Core.Type
builtins =
  Builtins
    { trace_i32 = Core.TFn [(Nothing, Core.Type Core.TI32)] Core.TUnit
    , trace_bool = Core.TFn [(Nothing, Core.Type Core.TBool)] Core.TUnit
    , add_i32 = Core.TFn [(Nothing, Core.Type Core.TI32), (Nothing, Core.Type Core.TI32)] Core.TI32
    }

data CheckEnv = CheckEnv
  { globals :: HashMap Text Core.Type
  , locals :: HashMap Core.Var Core.TypeOrKind
  }

data CheckError
  = KindMismatch
      -- | Expected
      Kind
      -- | Actual
      Kind
  | NotInScope
      -- | Variable name
      Var
  | -- | A value was given when a type was expected
    NotAType
      -- | Invalid argumentt
      Expr
  | TypeMismatch
      -- | Expected
      Type
      (Maybe Core.Type)
      -- | Actual candidates
      (Vector Type)
      (Maybe (Vector Core.Type))
  | IntegerOutOfBounds
      -- | Integer type
      Type
      -- | Invalid integer
      Integer
  | RecordMissingField
      -- | Missing field
      Text
  | NotAFunction
      -- | Invalid expression
      Expr
  | TooManyArguments
      -- | Function type
      Type
      -- | Provided argument count
      Int
  | -- | A type was given where a value was expected
    NotAValue
      -- | Invalid argument
      Type
  | Can'tInfer
      Expr
  deriving (Show, Eq)

newtype CheckT m a
  = CheckT
      ( ReaderT
          CheckEnv
          ( FreshT -- names
              ( FreshT -- vars
                  (ExceptT CheckError m)
              )
          )
          a
      )
  deriving (Functor, Applicative, Monad, MonadError CheckError)

runCheckT :: Monad m => CheckT m a -> m (Either CheckError a)
runCheckT (CheckT ma) =
  runExceptT . runFreshT . runFreshT $
    runReaderT ma CheckEnv{globals = foldMapBuiltins HashMap.singleton builtins, locals = mempty}

freshUnique :: Monad m => CheckT m Unique
freshUnique = CheckT $ do
  n <- lift . lift $ fresh
  pure $ U n

freshVar :: Monad m => CheckT m Core.Var
freshVar = Core.VUnique <$> freshUnique

withGlobals :: Monad m => HashMap Text Core.Type -> CheckT m a -> CheckT m a
withGlobals bs (CheckT ma) =
  CheckT $ local (\e -> e{globals = bs <> globals e}) ma

withLocal ::
  Monad m =>
  Core.Var ->
  Core.TypeOrKind ->
  CheckT m a ->
  CheckT m a
withLocal v ty ma =
  withLocals (HashMap.singleton v ty) ma

withLocals ::
  Monad m =>
  HashMap Core.Var Core.TypeOrKind ->
  CheckT m a ->
  CheckT m a
withLocals ls (CheckT ma) =
  CheckT $ local (\e -> e{locals = ls <> locals e}) ma

compBoundVars :: Core.Comp -> HashMap Core.Var Core.TypeOrKind
compBoundVars =
  foldl' (\acc (v, ty) -> HashMap.insert v ty acc) mempty . Core.compBoundVars

lookupVar :: HasCallStack => Monad m => Var -> CheckT m (Maybe (Core.Type, Core.Value))
lookupVar = withFrozenCallStack go
  where
    go :: HasCallStack => Monad m => Var -> CheckT m (Maybe (Core.Type, Core.Value))
    go (Gen u) = CheckT $ do
      varSort <-
        asks $
          fromMaybe (error $ "var " <> show u <> " not in scope") . HashMap.lookup (Core.VUnique u) . locals
      case varSort of
        Core.Type ty -> pure $ Just (ty, Core.Var $ Core.VUnique u)
        Core.Kind{} -> pure Nothing
    go (Src n) = CheckT $ do
      let v = Core.VNamed n
      mLocal <- asks $ HashMap.lookup v . locals
      case mLocal of
        Just (Core.Type ty) -> pure $ Just (ty, Core.Var v)
        Just Core.Kind{} -> pure Nothing
        Nothing -> do
          mTy <- asks $ HashMap.lookup n . globals
          case mTy of
            Just ty -> pure $ Just (ty, Core.Name n)
            Nothing -> pure Nothing

lookupTypeVar :: Monad m => Var -> CheckT m (Maybe (Core.Var, Core.TypeOrKind))
lookupTypeVar (Gen u) = CheckT $ do
  let v = Core.VUnique u
  varSort <- asks $ fromMaybe undefined . HashMap.lookup v . locals
  pure $ Just (v, varSort)
lookupTypeVar (Src n) = CheckT $ do
  let v = Core.VNamed n
  mLocal <- asks $ HashMap.lookup v . locals
  case mLocal of
    Just varSort -> pure $ Just (v, varSort)
    Nothing -> pure Nothing

checkModule :: Monad m => Module -> CheckT m Core.Module
checkModule (Module defs) = Core.Module . Vector.fromList <$> go (Vector.toList defs)
  where
    go [] = pure []
    go (def : rest) = do
      def' <- checkDefinition def
      rest' <- do
        let g =
              case def' of
                Core.Binding name (Just args) ret _ ->
                  HashMap.singleton name $ Core.TFn args ret
                Core.Binding name Nothing ty _ ->
                  HashMap.singleton name ty
        withGlobals g (go rest)
      pure (def' : rest')

withCheckedLocals ::
  Monad m =>
  [(Maybe Text, TypeOrKind)] ->
  CheckT m a ->
  CheckT m ([(Maybe Core.Var, Core.TypeOrKind)], a)
withCheckedLocals [] ma = (,) [] <$> ma
withCheckedLocals ((mName, typeOrKind) : rest) ma = do
  typeOrKind' <-
    case typeOrKind of
      Type ty -> Core.Type <$> checkType KType ty
      Kind ki -> pure $ Core.Kind ki
  v <-
    case mName of
      Just n -> pure $ Core.VNamed n
      Nothing -> freshVar
  withLocal v typeOrKind' $ do
    (rest', a) <- withCheckedLocals rest ma
    pure ((v <$ mName, typeOrKind') : rest', a)

checkDefinition :: Monad m => Definition -> CheckT m Core.Definition
checkDefinition (Binding name args retTy body)
  | Vector.length args == 0 = do
      retTy' <- checkType KType retTy
      body' <- checkExpr retTy' body
      pure $ Core.Binding name Nothing retTy' body'
  | otherwise = do
      let
        toCheck =
          mapMaybe
            ( \case
                PVar name' typeOrKind -> Just (name', typeOrKind)
                PUnit -> Nothing
            )
            (Vector.toList args)
      (args', (retTy', body')) <- withCheckedLocals toCheck $ do
        retTy' <- checkType KType retTy
        body' <- checkExpr retTy' body
        pure (retTy', body')
      pure $ Core.Binding name (Just $ Vector.fromList args') retTy' body'

unfoldTArrow :: Type -> ([(Maybe Text, TypeOrKind)], Type)
unfoldTArrow (TArrow a b) =
  let ~(a', b') = unfoldTArrow b in ((Nothing, Type a) : a', b')
unfoldTArrow (TForall name a b) =
  let ~(a', b') = unfoldTArrow b in ((Just name, Kind a) : a', b')
unfoldTArrow a =
  ([], a)

checkFunctionType :: Monad m => Type -> CheckT m Core.Type
checkFunctionType ty = do
  let (inTys, outTy) = unfoldTArrow ty
  (inTys', outTy') <- withCheckedLocals inTys $ checkType KType outTy
  pure $ Core.TFn (Vector.fromList inTys') outTy'

checkType :: Monad m => Kind -> Type -> CheckT m Core.Type
checkType _ TUnknown = error "TUnknown"
checkType ki (TVar v) = do
  mResult <- lookupTypeVar v
  case mResult of
    Just (v', typeOrKind) ->
      case typeOrKind of
        Core.Kind ki' ->
          if ki == ki'
            then pure $ Core.TVar v'
            else throwError $ KindMismatch ki ki'
        Core.Type{} ->
          throwError $ NotAType (Var v)
    Nothing ->
      throwError $ NotInScope v
checkType ki ty@TForall{}
  | KType <- ki = checkFunctionType ty
  | otherwise = throwError $ KindMismatch KType ki
checkType ki (TExists n a rest)
  | KType <- ki = do
      let v = Core.VNamed n
      rest' <- withLocal v (Core.Kind a) $ checkType KType rest
      pure $ Core.TExists v a rest'
  | otherwise = throwError $ KindMismatch KType ki
checkType ki ty@TArrow{}
  | KType <- ki = checkFunctionType ty
  | otherwise = throwError $ KindMismatch KType ki
checkType ki (TRecord fs)
  | KType <- ki = do
      fs' <- (traverse . traverse) (checkType KType) fs
      pure $ Core.TRecord fs'
  | otherwise = throwError $ KindMismatch KType ki
checkType ki TUnit
  | KType <- ki = pure Core.TUnit
  | otherwise = throwError $ KindMismatch KType ki
checkType ki TBool
  | KType <- ki = pure Core.TBool
  | otherwise = throwError $ KindMismatch KType ki
checkType ki TI32
  | KType <- ki = pure Core.TI32
  | otherwise = throwError $ KindMismatch KType ki
checkType ki TI64
  | KType <- ki = pure Core.TI64
  | otherwise = throwError $ KindMismatch KType ki
checkType ki (TArray ty)
  | KType <- ki = Core.TArray <$> checkType KType ty
  | otherwise = throwError $ KindMismatch KType ki

unfoldApp :: Expr -> (Expr, [Argument])
unfoldApp (App f x) = let ~(f', xs) = unfoldApp f in (f', xs ++ [x])
unfoldApp a = (a, [])

unfoldLam :: Expr -> ([(IsValueOrType, Text)], Expr)
unfoldLam (Lam nameSort name body) =
  let ~(params, body') = unfoldLam body in ((nameSort, name) : params, body')
unfoldLam e =
  ([], e)

substArguments :: [Core.Argument] -> [(Maybe Core.Var, Core.TypeOrKind)] -> Core.Type -> Core.Type
substArguments args argTys =
  Core.substType
    ( HashMap.fromList
        . mapMaybe
          ( \((mVar, _typeOrKind), arg) -> do
              v <- mVar
              case arg of
                Core.ArgType ty _ -> Just (v, ty)
                Core.ArgValue{} -> Nothing
          )
        $ zip argTys args
    )

inferExpr :: Monad m => Expr -> CheckT m (Core.Type, Core.Expr)
inferExpr (Var v) = do
  mResult <- lookupVar v
  case mResult of
    Nothing -> throwError $ NotInScope v
    Just (ty, value) -> pure (ty, pure value)
inferExpr (Let n mty val e) = do
  let v = Core.VNamed n

  (val'Ty, val') <-
    case mty of
      Nothing ->
        inferExpr val
      Just ty -> do
        ty' <- checkType KType ty
        v' <- checkExpr ty' val
        pure (ty', v')

  (e'Ty, e') <- withLocal v (Core.Type val'Ty) $ inferExpr e
  pure
    ( e'Ty
    , do
        val'val <- val'
        Core.tellComp $ Core.Let v val'Ty val'val
        e'
    )
inferExpr (Ann e t) = do
  t' <- checkType KType t
  e' <- checkExpr t' e
  pure (t', e')
inferExpr e@App{} | (f, xs) <- unfoldApp e = do
  (fTy, f') <- inferExpr f

  (argTys, outTy) <-
    case fTy of
      Core.TFn argTys outTy -> pure (argTys, outTy)
      _ -> throwError $ NotAFunction f

  case compare (length xs) (Vector.length argTys) of
    GT -> do
      {- Over-saturated application is valid when a saturated application returns a function.

      ## Example

      ```
      f : (a : Type) -> a |- f (i32 -> i32) 1 ~> bind g = f(i32 -> i32) in g(1) : i32
      ```

      `f` takes one argument (a type argument) so it's saturated by `f (i32 -> i32)`. The
      saturated application is bound and then applied to the remaining arguments.
      -}
      let (required, extra) = splitAt (Vector.length argTys) xs

      required' <- sequenceA <$> checkArguments [] (zip (Vector.toList argTys) required)

      let mkResultTy required'val = substArguments required'val (Vector.toList argTys) outTy

      u <- freshUnique
      let v = Core.VUnique u
      (rest'Ty, rest') <- withLocal v (Core.Type . mkResultTy $ Core.exprValue required') $ do
        let rest = foldl App (Var $ Gen u) extra
        inferExpr rest

      pure
        ( rest'Ty
        , do
            f'val <- f'
            required'val <- required'
            let resultTy = mkResultTy required'val
            Core.tellComp $ Core.Call v resultTy f'val (Vector.fromList required'val)
            rest'
        )
    EQ -> do
      {- Saturated application is bound.

      ## Example

      ```
      f : i32 -> i32 -> i32, x : i32, y : i32 |- f x y ~> (bind z = f(x, y) in z) : i32
      ```
      -}
      xs' <- sequenceA <$> checkArguments [] (zip (Vector.toList argTys) xs)

      let mkResultTy xs'val = substArguments xs'val (Vector.toList argTys) outTy

      u <- freshUnique
      let v = Core.VUnique u
      (rest'Ty, rest') <- withLocal v (Core.Type . mkResultTy $ Core.exprValue xs') $ do
        let rest = Var (Gen u)
        inferExpr rest

      pure
        ( rest'Ty
        , do
            f'val <- f'
            xs'val <- xs'
            Core.tellComp $
              Core.Call
                v
                (mkResultTy xs'val)
                f'val
                (Vector.fromList xs'val)
            rest'
        )
    LT -> do
      {- Partial application introduces a closure.

      ## Example

      ```
      f : i32 -> i32 -> i32, x : i32 |- f x ~> (\y -> f(x, y)) : i32 -> i32
      ```
      -}
      let (providedTys, leftoverTys) = Vector.splitAt (length xs) argTys

      xs' <- checkArguments [] (zip (Vector.toList providedTys) xs)

      let
        (leftoversTysInstantiated, outTyInstantiated) =
          Core.substFunctionType
            ( HashMap.fromList
                $ mapMaybe
                  ( \((mVar, _typeOrKind), arg) ->
                      case arg of
                        Core.ArgType ty _ -> (,ty) <$> mVar
                        Core.ArgValue{} ->
                          error $
                            "type depends on value (outTy = "
                              <> show outTy
                              <> ", args = "
                              <> show xs'
                              <> ")"
                  )
                $ zip
                  (Vector.toList providedTys)
                  (fmap Core.exprValue xs')
            )
            (Vector.toList leftoverTys)
            outTy

      leftoversLamParams <-
        traverse
          ( \(mVar, typeOrKind) ->
              (,typeOrKind) <$> maybe freshVar pure mVar
          )
          leftoversTysInstantiated

      let finalOutTy =
            Core.substType
              ( HashMap.fromList $
                  zipWith
                    (\v (v', _typeOrKind) -> (v, Core.TVar v'))
                    (mapMaybe fst leftoversTysInstantiated)
                    leftoversLamParams
              )
              outTyInstantiated

      u <- freshUnique
      let v = Core.VUnique u
      pure
        ( Core.TFn (Vector.fromList leftoversTysInstantiated) outTyInstantiated
        , do
            f'val <- f'
            xs'val <- sequenceA xs'
            pure $ Core.Lam (Vector.fromList $ (fmap . first) Just leftoversLamParams) $ do
              Core.tellComp $
                Core.Call
                  v
                  finalOutTy
                  f'val
                  ( Vector.fromList $
                      xs'val
                        <> fmap
                          ( \(v', typeOrKind) ->
                              case typeOrKind of
                                Core.Type ty ->
                                  Core.ArgValue (Core.Var v') ty
                                Core.Kind ki ->
                                  Core.ArgType (Core.TVar v') ki
                          )
                          leftoversLamParams
                  )

              pure $ Core.Var v
        )
inferExpr (Record fs) = do
  (fields', fs') <- fmap Vector.unzip . for fs $ \(field, e) -> do
    (t', e') <- inferExpr e
    pure ((field, t'), (field, e'))
  pure (Core.TRecord fields', Core.Record <$> traverse sequenceA fs')
inferExpr (Proj e f) = do
  (t, e') <- inferExpr e
  case t of
    Core.TRecord fields ->
      case Vector.find ((f ==) . fst) fields of
        Nothing ->
          throwError $ RecordMissingField f
        Just (_, ty) -> do
          pure
            ( ty
            , do
                e'val <- e'
                pure $ Core.Proj e'val f
            )
    _ ->
      throwError $ TypeMismatch (TRecord []) Nothing [Core.toSyntax t] (Just [t])
inferExpr (Bool b) = pure (Core.TBool, pure $ Core.Bool b)
inferExpr e@Lam{} = throwError $ Can'tInfer e
inferExpr e@Integer{} = throwError $ Can'tInfer e
inferExpr e@(Array as) =
  case Vector.uncons as of
    Nothing -> throwError $ Can'tInfer e
    Just (a, rest) -> do
      (a'Ty, a') <- inferExpr a
      rest' <- sequenceA <$> traverse (checkExpr a'Ty) rest
      pure (Core.TArray a'Ty, fmap (Core.Array a'Ty) $ Vector.cons <$> a' <*> rest')

typeMismatch :: MonadError CheckError m => Core.Type -> Core.Type -> m a
typeMismatch expected actual =
  throwError $
    TypeMismatch (Core.toSyntax expected) (Just expected) [Core.toSyntax actual] (Just [actual])

unify ::
  Monad m =>
  -- | Expected
  Core.Type ->
  -- | Actual
  Core.Type ->
  -- | Value to transform from actual type to expected type
  Core.Value ->
  -- | Returns 'Nothing' when the input expression hasn't changed
  CheckT m (Maybe Core.Expr)
unify expected@(Core.TVar v) actual _expr
  | Core.TVar v' <- actual, v == v' = pure Nothing
  | otherwise = typeMismatch expected actual
unify expected@(Core.TExists name k rest) actual expr
  | Core.TExists name' k' rest' <- actual
  , k == k' = do
      name'' <- Core.TVar <$> freshVar
      t <- freshVar
      x <- freshVar
      mX' <-
        unify
          (Core.substType (HashMap.singleton name name'') rest)
          (Core.substType (HashMap.singleton name' name'') rest')
          (Core.Var x)

      pure $
        mX' <&> \x' -> do
          Core.tellComp $ Core.LetExists (t, k') (x, rest') expr
          x'val <- x'
          pure $ Core.Pack expected (Core.TVar t) x'val
  | otherwise = typeMismatch expected actual
unify expected@(Core.TFn argSorts retTy) actual expr
  | Core.TFn argSorts' retTy' <- actual = do
      (Any changed, expr') <- unifyFunctions expected actual (argSorts, retTy) (argSorts', retTy') expr
      if changed then pure $ Just expr' else pure Nothing
  | otherwise = typeMismatch expected actual
unify expected@(Core.TRecord fields) actual expr
  | Core.TRecord fields' <- actual
  , Vector.length fields == Vector.length fields' = do
      (fs, Any fieldChanged) <-
        runWriterT $
          traverse
            ( \(fieldName, fieldTy) ->
                case Vector.find ((fieldName ==) . fst) fields' of
                  Nothing ->
                    typeMismatch expected actual
                  Just (_, fieldTy') -> do
                    let e = Core.Proj expr fieldName
                    mE' <- lift $ unify fieldTy fieldTy' e
                    case mE' of
                      Nothing -> pure (fieldName, pure e)
                      Just e' -> do
                        tell $ Any True
                        pure (fieldName, e')
            )
            fields

      if fieldChanged
        then pure . Just $ Core.Record <$> traverse sequenceA fs
        else pure Nothing
  | otherwise = typeMismatch expected actual
unify expected@Core.TUnit actual _expr
  | Core.TUnit <- actual = pure Nothing
  | otherwise = typeMismatch expected actual
unify expected@Core.TBool actual _expr
  | Core.TBool <- actual = pure Nothing
  | otherwise = typeMismatch expected actual
unify expected@Core.TI32 actual _expr
  | Core.TI32 <- actual = pure Nothing
  | otherwise = typeMismatch expected actual
unify expected@Core.TI64 actual _expr
  | Core.TI64 <- actual = pure Nothing
  | otherwise = typeMismatch expected actual
unify expected@(Core.TArray a) actual _expr
  | Core.TArray a' <- actual
  , a == a' =
      -- Subtyping on arrays requires a `map` operation. Do I want to add this?
      pure Nothing
  | otherwise = typeMismatch expected actual

unifyFunctions ::
  Monad m =>
  Core.Type ->
  Core.Type ->
  (Vector (Maybe Core.Var, Core.TypeOrKind), Core.Type) ->
  (Vector (Maybe Core.Var, Core.TypeOrKind), Core.Type) ->
  Core.Value ->
  CheckT m (Any, Core.Expr)
unifyFunctions expected actual (argSorts, retTy) (argSorts', retTy') expr
  | Vector.length argSorts == Vector.length argSorts' =
      fmap Tuple.swap . runWriterT $ do
        expr' <- do
          args <-
            traverse
              ( \((_mName', argSort'), (_mName, argSort)) ->
                  case (argSort', argSort) of
                    (Core.Type{}, Core.Kind{}) ->
                      typeMismatch expected actual
                    (Core.Kind{}, Core.Type{}) ->
                      typeMismatch expected actual
                    (Core.Kind k, Core.Kind k') -> do
                      if k == k'
                        then do
                          v <- lift freshVar
                          pure $ do
                            pure $ Core.ArgType (Core.TVar v) k
                        else typeMismatch expected actual
                    (Core.Type argTy', Core.Type argTy) -> do
                      v <- lift freshVar
                      let e = Core.Var v
                      mE' <- lift $ unify argTy' argTy e
                      case mE' of
                        Nothing ->
                          pure $ do
                            pure $ Core.ArgValue e argTy'
                        Just e' -> do
                          tell $ Any True
                          pure $ do
                            e'val <- e'
                            pure $ Core.ArgValue e'val argTy'
              )
              (Vector.zip argSorts' argSorts)

          v <- lift freshVar
          let e = Core.Var v

          mE' <- lift $ unify retTy retTy' e
          case mE' of
            Nothing -> do
              pure $ do
                pure e
            Just e' -> do
              tell $ Any True
              pure $ do
                argsVal <- sequenceA args
                Core.tellComp $ Core.Call v retTy' expr argsVal
                e'

        pure $ do
          pure $ Core.Lam argSorts expr'
  | otherwise = typeMismatch expected actual

checkExpr ::
  Monad m =>
  Core.Type ->
  Expr ->
  CheckT m Core.Expr
checkExpr ty (Let name mty val rest) = do
  let v = Core.VNamed name

  (val'Ty, val') <-
    case mty of
      Nothing -> inferExpr val
      Just vTy -> do
        vTy' <- checkType KType vTy
        v' <- checkExpr vTy' val
        pure (vTy', v')

  rest' <- withLocal v (Core.Type val'Ty) $ checkExpr ty rest

  pure $ do
    v'val <- val'
    Core.tellComp $ Core.Let v val'Ty v'val
    rest'
checkExpr ty e@(Lam nameSort name _) = do
  (paramTys, outTy) <-
    case ty of
      Core.TFn paramTys outTy ->
        pure (paramTys, outTy)
      _ ->
        case nameSort of
          IsValue ->
            throwError $ TypeMismatch (Core.toSyntax ty) (Just ty) [TArrow TUnknown TUnknown] Nothing
          IsType ->
            throwError $ TypeMismatch (Core.toSyntax ty) (Just ty) [TForall name KUnknown TUnknown] Nothing

  let (params, body) = unfoldLam e
  let
    params' =
      fmap
        (\((_, paramName), (_, typeOrKind)) -> (Core.VNamed paramName, typeOrKind))
        (zip params (Vector.toList paramTys))

  body' <- do
    let ls = HashMap.fromList params'
    withLocals ls $
      case compare (length params) (length paramTys) of
        GT -> throwError $ TooManyArguments (Core.toSyntax ty) (length params)
        EQ -> checkExpr outTy body
        LT -> do
          extraParams <-
            traverse
              ( \(mVar, paramSort) -> do
                  v <-
                    maybe
                      (Gen <$> freshUnique)
                      ( \case
                          Core.VUnique u -> pure $ Gen u
                          Core.VNamed n -> pure $ Src n
                      )
                      mVar
                  case paramSort of
                    Core.Type{} ->
                      pure . ArgValue $ Var v
                    Core.Kind{} ->
                      pure . ArgType $ TVar v
              )
              (Vector.drop (length params) paramTys)
          checkExpr outTy (foldl App body extraParams)

  pure $ do
    pure $ Core.Lam (Vector.fromList $ (fmap . first) Just params') body'
checkExpr Core.TI32 (Integer i)
  | fromIntegral (minBound :: Int32) <= i && i <= fromIntegral (maxBound :: Int32) =
      pure $ do
        pure $ Core.I32 (fromIntegral i)
  | otherwise = throwError $ IntegerOutOfBounds TI32 i
checkExpr Core.TI64 (Integer i)
  | fromIntegral (minBound :: Int64) <= i && i <= fromIntegral (maxBound :: Int64) =
      pure $ do
        pure $ Core.I64 (fromIntegral i)
  | otherwise = throwError $ IntegerOutOfBounds TI64 i
checkExpr ty Integer{} = throwError $ TypeMismatch (Core.toSyntax ty) (Just ty) [TI32, TI64] Nothing
checkExpr ty (Array as)
  | Core.TArray itemTy <- ty = do
      as' <- sequenceA <$> traverse (checkExpr itemTy) as
      pure (Core.Array itemTy <$> as')
  | otherwise =
      throwError $ TypeMismatch (Core.toSyntax ty) (Just ty) [TArray TUnknown] Nothing
checkExpr ty e = do
  (ty', e') <- inferExpr e
  mE'' <- unify ty ty' $ Core.exprValue e'
  case mE'' of
    Nothing -> pure e'
    Just e'' -> pure $ do
      _e'val <- e'
      e''

checkArguments ::
  Monad m =>
  HashMap Core.Var Core.Argument ->
  [((Maybe Core.Var, Core.TypeOrKind), Argument)] ->
  CheckT m [Core.Expr' Core.Argument]
checkArguments _ [] = pure []
checkArguments seen (((mVar, argSort), arg) : rest) = do
  case argSort of
    Core.Type argTy ->
      case arg of
        ArgValue exprArg -> do
          let
            argTy' =
              Core.substType
                ( fmap
                    ( \case
                        Core.ArgValue{} -> error "argument type depends on value"
                        Core.ArgType ty _ -> ty
                    )
                    seen
                )
                argTy

          arg' <- checkExpr argTy' exprArg
          rest' <-
            checkArguments
              ( maybe
                  id
                  (\v -> HashMap.insert v (Core.ArgValue (Core.exprValue arg') argTy'))
                  mVar
                  seen
              )
              rest
          pure $ fmap (\arg'val -> Core.ArgValue arg'val argTy') arg' : rest'
        ArgType tyArg ->
          throwError $ NotAValue tyArg
    Core.Kind argKind ->
      case arg of
        ArgType tyArg -> do
          arg' <- checkType argKind tyArg
          rest' <-
            checkArguments
              ( maybe
                  id
                  (\v -> HashMap.insert v (Core.ArgType arg' argKind))
                  mVar
                  seen
              )
              rest
          pure $ pure (Core.ArgType arg' argKind) : rest'
        ArgValue exprArg ->
          throwError $ NotAType exprArg
