{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Compiler.Core where

import Compiler.Kind (Kind (..))
import Compiler.Syntax (Unique)
import qualified Compiler.Syntax as Syntax
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable (..))
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack, withFrozenCallStack)

newtype Module
  = Module (Vector Definition)
  deriving (Show, Eq)

data TypeOrKind = Type Type | Kind Kind
  deriving (Show, Eq)

data Definition
  = Binding Text (Maybe (Vector (Maybe Var, TypeOrKind))) Type Expr
  deriving (Show, Eq)

data Var
  = VNamed Text
  | VUnique Unique
  deriving (Show, Eq, Generic, Hashable)

varToSyntax :: Var -> Text
varToSyntax (VNamed n) = n
varToSyntax (VUnique i) = Text.pack $ "unique%" ++ show i

data Type
  = TVar Var
  | TExists Var Kind Type
  | TFn (Vector (Maybe Var, TypeOrKind)) Type
  | TRecord (Vector (Text, Type))
  | TUnit
  | TBool
  | TI32
  | TI64
  | TArray Type
  deriving (Show, Eq)

toSyntax :: Type -> Syntax.Type
toSyntax (TVar v) = Syntax.TVar . Syntax.Src $ varToSyntax v
toSyntax (TExists v k t) =
  Syntax.TExists (varToSyntax v) k (toSyntax t)
toSyntax (TFn args ret) =
  foldr
    ( \(mVar, typeOrKind) rest ->
        case typeOrKind of
          Type ty -> Syntax.TArrow (toSyntax ty) rest
          Kind ki ->
            case mVar of
              Just v -> Syntax.TForall (varToSyntax v) ki rest
              Nothing -> error "unnamed type variable"
    )
    (toSyntax ret)
    args
toSyntax (TRecord fields) =
  Syntax.TRecord $ (fmap . fmap) toSyntax fields
toSyntax TUnit = Syntax.TUnit
toSyntax TBool = Syntax.TBool
toSyntax TI32 = Syntax.TI32
toSyntax TI64 = Syntax.TI64
toSyntax (TArray ty) = Syntax.TArray (toSyntax ty)

data Argument
  = ArgValue Value Type
  | ArgType Type Kind
  deriving (Show, Eq)

data Value
  = Name Text
  | Var Var
  | Lam (Vector (Maybe Var, TypeOrKind)) Expr
  | Record (Vector (Text, Value))
  | -- TODO: this shouldn't really be here
    Proj Value Text
  | Pack Type Type Value
  | Bool Bool
  | I32 Int32
  | I64 Int64
  | Array
      -- | Element type
      Type
      -- | Elements
      (Vector Value)
  deriving (Show, Eq)

data Comp
  = Let Var Type Value
  | -- | A saturated function call.
    --
    --   'Call' binds its result to correctly order effects, such as non-termination.
    --
    --   Take these definitions:
    --
    --   @
    --   abort : (type a : Type) -> a
    --
    --   f : (i32 -> i32) -> i32
    --   f _ = 1
    --   @
    --
    --   For a function like `f (abort (type i32 -> i32 -> i32) 5)` to be correct
    --   in a call-by-value setting, it needs to have the same meaning as
    --   `abort (type i32 -> i32 -> i32)`. Naively, this would be lowered into
    --   `f (\x -> abort (type i32 -> i32 -> i32) 5 x)`, which would evaluate to
    --   `1` instead of `_|_`.
    --
    --   With saturated call binding, `f (abort (type i32 -> i32 -> i32) 5)`
    --   is lowered to
    --
    --   ```
    --   call g = abort (type i32 -> i32 -> i32);
    --   let g' = \x -> g 5 x;
    --   call f' = f g';
    --   f'
    --   ```
    --
    --   This forces the `abort` to occur before anything else.
    --
    --   Call-by-push-value provides the semantic foundation for this decision.
    --   A function like `abort` has CBPV type `U ((a : Type) -> F a)`, and
    --   `f : U (U (i32 -> i32 -> F i32) -> F i32)`.
    --   `abort (i32 -> i32 -> i32) : i32 -> i32 -> i32` becomes
    --   `abort (U (i32 -> i32 -> F i32)) : F (U (i32 -> i32 -> F i32))`.
    --
    --   ```
    --   bind g : U (i32 -> i32 -> F i32) = abort (U (i32 -> i32 -> F i32));
    --   force(f) g : F i32
    --   ```
    Call Var Type Value (Vector Argument)
  | LetExists (Var, Kind) (Var, Type) Value
  deriving (Show, Eq)

compBoundVars :: Comp -> [(Var, TypeOrKind)]
compBoundVars (Let v ty _val) = [(v, Type ty)]
compBoundVars (Call v ty _f _xs) = [(v, Type ty)]
compBoundVars (LetExists (v1, tyKind) (v2, exprType) _val) = [(v1, Kind tyKind), (v2, Type exprType)]

data Expr' a = Expr {exprComps :: Vector Comp, exprValue :: a}
  deriving (Show, Eq, Functor)

type Expr = Expr' Value

instance Applicative Expr' where
  pure = Expr mempty
  (<*>) (Expr cs f) (Expr cs' x) = Expr (cs <> cs') (f x)

instance Monad Expr' where
  (>>=) (Expr cs a) f = let Expr cs' b = f a in Expr (cs <> cs') b

tellComp :: Comp -> Expr' ()
tellComp c = Expr (Vector.singleton c) ()

substType :: HashMap Var Type -> Type -> Type
substType ts t@(TVar v) =
  fromMaybe t $ HashMap.lookup v ts
substType ts (TExists v k rest) =
  TExists v k (substType (HashMap.delete v ts) rest)
substType ts (TFn args ret) =
  let (args', ret') = substFunctionType ts (Vector.toList args) ret
  in TFn (Vector.fromList args') ret'
substType ts (TRecord fields) =
  TRecord $ (fmap . fmap) (substType ts) fields
substType _ TUnit = TUnit
substType _ TBool = TBool
substType _ TI32 = TI32
substType _ TI64 = TI64
substType ts (TArray a) = TArray (substType ts a)

substFunctionType ::
  HashMap Var Type ->
  [(Maybe Var, TypeOrKind)] ->
  Type ->
  ([(Maybe Var, TypeOrKind)], Type)
substFunctionType ts [] ret = ([], substType ts ret)
substFunctionType ts ((mVar, argSort) : args) ret =
  let (args', ret') = substFunctionType (maybe id HashMap.delete mVar ts) args ret
  in case argSort of
      Type argTy ->
        let argTy' = substType ts argTy
        in ((mVar, Type argTy') : args', ret')
      Kind argKind ->
        ((mVar, Kind argKind) : args', ret')

typeOf ::
  HasCallStack =>
  HashMap Text Type ->
  HashMap Var TypeOrKind ->
  Expr ->
  TypeOrKind
typeOf x1 x2 (Expr x4 x5) = withFrozenCallStack (go x1 x2 (Vector.toList x4) x5)
  where
    go ::
      HasCallStack =>
      HashMap Text Type ->
      HashMap Var TypeOrKind ->
      [Comp] ->
      Value ->
      TypeOrKind
    go ctx vars [] value =
      typeOfValue ctx vars value
    go ctx vars (Let v ty _val : rest) value =
      go ctx (HashMap.insert v (Type ty) vars) rest value
    go ctx vars (Call v ty _a _bs : rest) value =
      go ctx (HashMap.insert v (Type ty) vars) rest value
    go ctx vars (LetExists (tyVar, tyKind) (exprVar, exprTy) _val : rest) value =
      go
        ctx
        (HashMap.fromList [(tyVar, Kind tyKind), (exprVar, Type exprTy)] <> vars)
        rest
        value

typeOfValue ::
  HasCallStack =>
  HashMap Text Type ->
  HashMap Var TypeOrKind ->
  Value ->
  TypeOrKind
typeOfValue = withFrozenCallStack go
  where
    go ::
      HasCallStack =>
      HashMap Text Type ->
      HashMap Var TypeOrKind ->
      Value ->
      TypeOrKind
    go ctx _ (Name name) =
      maybe
        (error $ "missing type for " <> show name)
        Type
        (HashMap.lookup name ctx)
    go _ctx vars (Var v) =
      fromMaybe undefined $ HashMap.lookup v vars
    go ctx vars (Lam args body) =
      let vars' =
            HashMap.fromList
              (mapMaybe (\(mVar, typeOrKind) -> fmap (\var -> (var, typeOrKind)) mVar) $ Vector.toList args)
              <> vars
      in typeOf ctx vars' body
    go ctx vars (Record fields) =
      Type . TRecord $
        (fmap . fmap)
          ( \fieldExpr ->
              case go ctx vars fieldExpr of
                Type ty -> ty
                Kind{} -> error "type in record field"
          )
          fields
    go ctx vars (Proj record field) =
      case go ctx vars record of
        Type (TRecord fields) ->
          maybe
            (error $ "record missing field " <> show field)
            (Type . snd)
            (Vector.find ((field ==) . fst) fields)
        _ -> error "projecting from non-record"
    go _ctx _vars (Pack ty _a _b) = Type ty
    go _ctx _vars Bool{} = Type TBool
    go _ctx _vars I32{} = Type TI32
    go _ctx _vars I64{} = Type TI64
    go _ctx _vars (Array ty _) = Type $ TArray ty

kindOf ::
  HasCallStack =>
  HashMap Var TypeOrKind ->
  Type ->
  TypeOrKind
kindOf = withFrozenCallStack go
  where
    go ::
      HasCallStack =>
      HashMap Var TypeOrKind ->
      Type ->
      TypeOrKind
    go vars (TVar v) = fromMaybe undefined $ HashMap.lookup v vars
    go _vars (TExists _name _k _rest) = Kind KType
    go _vars (TFn _ _) = Kind KType
    go _vars TRecord{} = Kind KType
    go _vars TUnit = Kind KType
    go _vars TBool = Kind KType
    go _vars TI32 = Kind KType
    go _vars TI64 = Kind KType
    go _vars TArray{} = Kind KType

data Multiplicity = Zero | One | Many
  deriving (Show, Eq)

instance Semigroup Multiplicity where
  Zero <> m = m
  One <> Zero = One
  One <> One = Many
  One <> Many = Many
  Many <> _ = Many

instance Monoid Multiplicity where
  mempty = Zero

data FreeVars = FreeVars (HashMap Var Multiplicity) (DList Var)

toListFreeVars :: FreeVars -> [(Var, Multiplicity)]
toListFreeVars (FreeVars mults vars) =
  (\var -> (var, mults HashMap.! var)) <$> DList.toList vars

emptyFreeVars :: FreeVars
emptyFreeVars = FreeVars HashMap.empty []

singletonFreeVars :: (Var, Multiplicity) -> FreeVars
singletonFreeVars (var, m) = FreeVars (HashMap.singleton var m) (DList.singleton var)

snocFreeVars :: FreeVars -> (Var, Multiplicity) -> FreeVars
snocFreeVars (FreeVars mults vars) (var, m) =
  case HashMap.lookup var mults of
    Nothing -> FreeVars (HashMap.insert var m mults) (DList.snoc vars var)
    Just m' -> FreeVars (HashMap.insert var (m' <> m) mults) vars

instance Semigroup FreeVars where
  a <> b = foldl' snocFreeVars a (toListFreeVars b)

instance Monoid FreeVars where
  mempty = emptyFreeVars

data TypeOrTermVar = TypeVar | TermVar

freeVarsExpr :: (Var -> Maybe Var) -> Expr -> FreeVars
freeVarsExpr f = foldFreeVarsExpr g
  where
    g t v =
      let
        m = case t of
          TypeVar -> Zero
          TermVar -> One
      in
        foldMap (\v' -> singletonFreeVars (v', m)) (f v)

freeVarsValue :: (Var -> Maybe Var) -> Value -> FreeVars
freeVarsValue f = foldFreeVarsValue g
  where
    g t v =
      let
        m = case t of
          TypeVar -> Zero
          TermVar -> One
      in
        foldMap (\v' -> singletonFreeVars (v', m)) (f v)

freeVarsType :: (Var -> Maybe Var) -> Type -> FreeVars
freeVarsType f = foldFreeVarsType g
  where
    g t v =
      let
        m = case t of
          TypeVar -> Zero
          TermVar -> One
      in
        foldMap (\v' -> singletonFreeVars (v', m)) (f v)

foldFreeVarsType ::
  Monoid m =>
  (TypeOrTermVar -> Var -> m) ->
  Type ->
  m
foldFreeVarsType onVar (TVar v) =
  onVar TypeVar v
foldFreeVarsType onVar (TExists name _k rest) =
  foldFreeVarsType
    ( \t v ->
        if v == name
          then mempty
          else onVar t v
    )
    rest
foldFreeVarsType onVar (TFn args ret) =
  foldFreeVarsArgs onVar (Vector.toList args)
    <> foldFreeVarsType
      ( \t v ->
          if any ((Just v ==) . fst) args
            then mempty
            else onVar t v
      )
      ret
foldFreeVarsType onVar (TRecord fields) =
  (foldMap . foldMap) (foldFreeVarsType onVar) fields
foldFreeVarsType _onVar TUnit = mempty
foldFreeVarsType _onVar TBool = mempty
foldFreeVarsType _onVar TI32 = mempty
foldFreeVarsType _onVar TI64 = mempty
foldFreeVarsType onVar (TArray a) = foldFreeVarsType onVar a

foldFreeVarsArgs ::
  Monoid m =>
  (TypeOrTermVar -> Var -> m) ->
  [(Maybe Var, TypeOrKind)] ->
  m
foldFreeVarsArgs _onVar [] = mempty
foldFreeVarsArgs onVar ((mName, typeOrKind) : args) =
  ( case typeOrKind of
      Type ty -> foldFreeVarsType onVar ty
      Kind{} -> mempty
  )
    <> foldFreeVarsArgs
      ( \t v ->
          if Just v == mName
            then mempty
            else onVar t v
      )
      args

foldFreeVarsExpr ::
  Monoid m =>
  (TypeOrTermVar -> Var -> m) ->
  Expr ->
  m
foldFreeVarsExpr onVar (Expr cs val) =
  go onVar (Vector.toList cs) val
  where
    go onVar' [] value = foldFreeVarsValue onVar' value
    go onVar' (comp : comps) value =
      foldFreeVarsComp onVar' comp
        <> let !vars = compBoundVars comp
           in go
                ( \t v ->
                    if any ((v ==) . fst) vars
                      then mempty
                      else onVar' t v
                )
                comps
                value

foldFreeVarsComp ::
  Monoid m =>
  (TypeOrTermVar -> Var -> m) ->
  Comp ->
  m
foldFreeVarsComp onVar (Let _v ty val) =
  foldFreeVarsType onVar ty <> foldFreeVarsValue onVar val
foldFreeVarsComp onVar (Call _v ty a bs) =
  foldFreeVarsType onVar ty
    <> foldFreeVarsValue onVar a
    <> foldMap
      ( \case
          ArgValue e t -> foldFreeVarsValue onVar e <> foldFreeVarsType onVar t
          ArgType t _k -> foldFreeVarsType onVar t
      )
      bs
foldFreeVarsComp onVar (LetExists _ty _expr val) =
  foldFreeVarsValue onVar val

foldFreeVarsValue ::
  Monoid m =>
  (TypeOrTermVar -> Var -> m) ->
  Value ->
  m
foldFreeVarsValue onVar (Var v) =
  onVar TermVar v
foldFreeVarsValue onVar (Lam args body) =
  foldFreeVarsArgs onVar (Vector.toList args)
    <> foldFreeVarsExpr
      ( \t v ->
          if any ((Just v ==) . fst) args
            then mempty
            else onVar t v
      )
      body
foldFreeVarsValue onVar (Record fields) =
  (foldMap . foldMap) (foldFreeVarsValue onVar) fields
foldFreeVarsValue onVar (Proj record _field) =
  foldFreeVarsValue onVar record
foldFreeVarsValue onVar (Pack _ a b) =
  foldFreeVarsType onVar a <> foldFreeVarsValue onVar b
foldFreeVarsValue _onVar Name{} = mempty
foldFreeVarsValue _onVar Bool{} = mempty
foldFreeVarsValue _onVar I32{} = mempty
foldFreeVarsValue _onVar I64{} = mempty
foldFreeVarsValue onVar (Array ty as) =
  foldFreeVarsType onVar ty
    <> foldMap (foldFreeVarsValue onVar) as
