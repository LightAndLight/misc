{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Compiler.Plugin (plugin) where

import qualified Control.Applicative
import Control.Monad (when)
import Data.Data (Data)
import Data.Maybe (fromJust)
import Data.Traversable (for)
import Data.Typeable (cast)
import GHC.Builtin.Types.Prim (intPrimTy)
import qualified GHC.Core as Core
import GHC.Core.TyCo.Compare (eqType)
import GHC.Data.Maybe (MaybeT, fromMaybe, liftMaybeT, runMaybeT)
import GHC.Driver.Config.Diagnostic (initDiagOpts)
import GHC.Driver.Errors.Types (GhcMessage (..))
import GHC.Plugins hiding (Expr)
import GHC.Runtime.Loader (lookupRdrNameInModuleForPlugins)
import GHC.Tc.Utils.TcType (isBoolTy, isIntTy)
import GHC.Types.Error (DiagnosticMessage (..), UnknownDiagnostic (UnknownDiagnostic), mkSimpleDecorated)
import GHC.Types.TyThing (MonadThings (lookupTyCon), lookupId)
import GHC.Utils.Error (mkMsgEnvelope, pprLocMsgEnvelopeDefault)
import Generics.SYB (everywhereM)
import Prelude hiding (mod)

data Env = Env
  { quoteVar :: Id
  , quotedCtor :: Id
  , toStringVar :: Id
  , undefinedVar :: Id
  , exprIntCtor :: Id
  , exprLamCtor :: Id
  , exprAppCtor :: Id
  , exprVarCtor :: Id
  , exprAddCtor :: Id
  , exprIfThenElseCtor :: Id
  , exprBoolCtor :: Id
  , exprLtCtor :: Id
  , exprCaseCtor :: Id
  , exprCharCtor :: Id
  , exprPairCtor :: Id
  , exprToStringCtor :: Id
  , branchTyCon :: TyCon
  , branchBranchCtor :: Id
  , patternPDefaultCtor :: Id
  , patternPIntCtor :: Id
  , patternPPairCtor :: Id
  , patternPUnitCtor :: Id
  , indexZCtor :: Id
  , indexSCtor :: Id
  , primIntAdd :: Id
  , primIntLt :: Id
  }

plugin :: Plugin
plugin =
  defaultPlugin
    { installCoreToDos = \_cli todos -> do
        -- The list of plugin passes is in reverse order of application;
        -- the first pass in the list is run last.
        undefinedVar <- findVar "Prelude" "undefined"
        quoteVar <- findVar "Compiler.Plugin.Interface" "quote"
        quotedCtor <- findCtor "Compiler.Plugin.Interface" "Quoted"
        toStringVar <- findVar "Compiler.Plugin.Interface" "toString"
        exprIntCtor <- findCtor "Compiler.Plugin.Interface" "Int"
        exprLamCtor <- findCtor "Compiler.Plugin.Interface" "Lam"
        exprAppCtor <- findCtor "Compiler.Plugin.Interface" "App"
        exprVarCtor <- findCtor "Compiler.Plugin.Interface" "Var"
        exprAddCtor <- findCtor "Compiler.Plugin.Interface" "Add"
        exprIfThenElseCtor <- findCtor "Compiler.Plugin.Interface" "IfThenElse"
        exprBoolCtor <- findCtor "Compiler.Plugin.Interface" "Bool"
        exprLtCtor <- findCtor "Compiler.Plugin.Interface" "Lt"
        exprCaseCtor <- findCtor "Compiler.Plugin.Interface" "Case"
        exprCharCtor <- findCtor "Compiler.Plugin.Interface" "Char"
        exprToStringCtor <- findCtor "Compiler.Plugin.Interface" "ToString"
        exprPairCtor <- findCtor "Compiler.Plugin.Interface" "Pair"
        branchBranchCtor <- findCtor "Compiler.Plugin.Interface" "Branch"
        branchTyCon <- findTyCon "Compiler.Plugin.Interface" "Branch"
        patternPDefaultCtor <- findCtor "Compiler.Plugin.Interface" "PDefault"
        patternPIntCtor <- findCtor "Compiler.Plugin.Interface" "PInt"
        patternPPairCtor <- findCtor "Compiler.Plugin.Interface" "PPair"
        patternPUnitCtor <- findCtor "Compiler.Plugin.Interface" "PUnit"
        indexZCtor <- findCtor "Compiler.Plugin.Interface" "Z"
        indexSCtor <- findCtor "Compiler.Plugin.Interface" "S"
        primIntAdd <- findVar "GHC.Exts" "+#"
        primIntLt <- findVar "GHC.Exts" "<#"
        let env =
              Env
                { quoteVar
                , quotedCtor
                , toStringVar
                , undefinedVar
                , exprIntCtor
                , exprLamCtor
                , exprAppCtor
                , exprVarCtor
                , exprAddCtor
                , exprIfThenElseCtor
                , exprBoolCtor
                , exprLtCtor
                , exprCaseCtor
                , exprCharCtor
                , exprToStringCtor
                , exprPairCtor
                , branchTyCon
                , branchBranchCtor
                , patternPDefaultCtor
                , patternPIntCtor
                , patternPPairCtor
                , patternPUnitCtor
                , indexZCtor
                , indexSCtor
                , primIntAdd
                , primIntLt
                }
        let todos' = todos ++ [CoreDoPluginPass "compiler-plugin-pass" (pass env)]
        -- let todos' = CoreDoPluginPass "compiler-plugin-pass" (pass env) : todos
        -- let todos' = take 1 todos ++ [CoreDoPluginPass "compiler-plugin-pass" (pass env)] ++ drop 1 todos
        -- let todos' = take (length todos - 1) todos ++ [CoreDoPluginPass "compiler-plugin-pass" (pass env)] ++ drop (length todos - 1) todos
        putMsg $ ppr todos'
        pure todos'
    }

-- Reference: https://github.com/compiling-to-categories/concat/blob/5d670b96cd770c6b96f55429bd3a830322ffaf16/inline/src/ConCat/Inline/Plugin.hs#L56
findCtor :: String -> String -> CoreM Id
findCtor mod str = do
  hsc_env <- getHscEnv
  mInfo <- liftIO (lookupRdrNameInModuleForPlugins hsc_env (mkModuleName mod) (Unqual (mkDataOcc str)))
  maybe (panic err) (lookupId . fst) mInfo
 where
  err = "findCtor: couldn't find " ++ str ++ " in " ++ moduleNameString (mkModuleName mod)

findVar :: String -> String -> CoreM Id
findVar mod str = do
  hsc_env <- getHscEnv
  mInfo <- liftIO (lookupRdrNameInModuleForPlugins hsc_env (mkModuleName mod) (Unqual (mkVarOcc str)))
  maybe (panic err) (lookupId . fst) mInfo
 where
  err = "findVar: couldn't find " ++ str ++ " in " ++ moduleNameString (mkModuleName mod)

findTyCon :: String -> String -> CoreM TyCon
findTyCon mod str = do
  hsc_env <- getHscEnv
  mInfo <- liftIO (lookupRdrNameInModuleForPlugins hsc_env (mkModuleName mod) (Unqual (mkTcOcc str)))
  maybe (panic err) (lookupTyCon . fst) mInfo
 where
  err = "findTyCon: couldn't find " ++ str ++ " in " ++ moduleNameString (mkModuleName mod)

logBinds :: Bool -> (CoreBind -> CoreM CoreBind) -> CoreBind -> CoreM CoreBind
logBinds cond f b = do
  when cond . putMsg $ hcat [text "input: ", ppr b]
  b' <- f b
  when cond . putMsg $ hcat [text "output: ", ppr b']
  pure b'

pass :: Env -> CorePluginPass
pass env = bindsOnlyPass $ \binds ->
  traverse (logBinds False $ bindPass binds) binds
 where
  bindPass :: [CoreBind] -> CoreBind -> CoreM CoreBind
  bindPass localBinds (NonRec name expr) =
    NonRec name <$> everywhereM (onCoreExprM $ exprPass localBinds) expr
  bindPass localBinds (Rec binds) = do
    Rec
      <$> for
        binds
        ( \(name, expr) ->
            (,) name <$> everywhereM (onCoreExprM $ exprPass localBinds) expr
        )

  onCoreExprM :: (Monad m) => (CoreExpr -> m CoreExpr) -> forall a. (Data a) => a -> m a
  onCoreExprM f a =
    case cast a of
      Nothing -> pure a
      Just a' -> fromJust . cast <$> f a'

  exprPass :: [CoreBind] -> CoreExpr -> CoreM CoreExpr
  exprPass localBinds expr@(Core.collectArgs -> (Core.Var var, args)) = do
    let name = varName var
    case filter (\case Type _ -> False; _ -> True) args of
      [_callStack, arg] ->
        if and
          [ fmap (moduleNameString . moduleName) (nameModule_maybe name) == Just "Compiler.Plugin.Interface"
          , var == quoteVar env -- occNameString (nameOccName name) == "quote"
          ]
          then fromMaybe expr <$> runMaybeT (quotedCore localBinds arg)
          else pure expr
      _ -> do
        -- putMsg $ hcat [text "skipped: ", ppr expr]
        pure expr
  exprPass _ expr = pure expr

  quotedCore :: [CoreBind] -> CoreExpr -> MaybeT CoreM CoreExpr
  quotedCore localBinds expr = do
    expr' <- quoteCoreExpr localBinds [] expr
    pure
      $ Core.App
        ( Core.App
            ( Core.App
                (Core.Var $ quotedCtor env)
                (Core.Type (exprType expr))
            )
            expr'
        )
        expr

  mkIndex :: [(CoreBndr, Type)] -> CoreBndr -> MaybeT CoreM (Type, CoreExpr)
  mkIndex [] var = do
    liftMaybeT . putMsg $ hcat [ppr $ nameSrcSpan $ varName var, text ": variable \"", ppr . nameOccName $ varName var, text "\" not in scope"]
    Control.Applicative.empty
  mkIndex ((var, ty) : ctx) var' =
    if var == var'
      then
        pure
          ( ty
          , mkCoreApps
              (Core.Var $ indexZCtor env)
              [ Core.Type $ mkPromotedListTy liftedTypeKind $ fmap snd ctx
              , Core.Type ty
              ]
          )
      else do
        (ty', index) <- mkIndex ctx var'
        pure
          ( ty'
          , mkCoreApps
              (Core.Var $ indexSCtor env)
              [ Core.Type $ mkPromotedListTy liftedTypeKind $ fmap snd ctx
              , Core.Type ty'
              , Core.Type ty
              , index
              ]
          )

  quoteCoreExpr :: [CoreBind] -> [(CoreBndr, Type)] -> CoreExpr -> MaybeT CoreM CoreExpr
  -- boolean literals
  quoteCoreExpr _localBinds context expr@(Core.Var var)
    | var == dataConWorkId trueDataCon || var == dataConWorkId falseDataCon =
        pure
          $ mkCoreApps
            (Core.Var $ exprBoolCtor env)
            [ Core.Type $ mkPromotedListTy liftedTypeKind $ fmap snd context
            , expr
            ]
  -- unboxed int literals
  quoteCoreExpr _localBinds context expr@(Core.Lit (LitNumber LitNumInt _)) =
    pure
      $ mkCoreApps
        (Core.Var $ exprIntCtor env)
        [ Core.Type $ mkPromotedListTy liftedTypeKind $ fmap snd context
        , mkCoreConApps intDataCon [expr]
        ]
  -- pairs
  quoteCoreExpr localBinds context expr
    | (Core.Var name, [aTy, bTy, a, b]) <- collectArgs expr
    , name == dataConWorkId (tupleDataCon Boxed 2) = do
        a' <- quoteCoreExpr localBinds context a
        b' <- quoteCoreExpr localBinds context b
        pure
          $ mkCoreApps
            (Core.Var $ exprPairCtor env)
            [ Core.Type $ mkPromotedListTy liftedTypeKind $ fmap snd context
            , aTy
            , bTy
            , a'
            , b'
            ]
  -- boxed int literals
  quoteCoreExpr localBinds context expr@(Core.App (Core.Var var) x)
    | var == dataConWorkId intDataCon =
        case x of
          Core.Lit (LitNumber LitNumInt _) ->
            pure
              $ mkCoreApps
                (Core.Var $ exprIntCtor env)
                [ Core.Type $ mkPromotedListTy liftedTypeKind $ fmap snd context
                , expr
                ]
          Core.App{}
            | (Core.Var f, [a, b]) <- collectArgs x ->
                do
                  if f == primIntAdd env
                    then do
                      a' <- quoteCoreExpr localBinds context a
                      b' <- quoteCoreExpr localBinds context b
                      pure
                        $ mkCoreApps
                          (Core.Var $ exprAddCtor env)
                          [ Core.Type $ mkPromotedListTy liftedTypeKind $ fmap snd context
                          , a'
                          , b'
                          ]
                    else do
                      liftMaybeT . putMsg $ hcat [text "unrecognised binop: ", hsep [ppr f, ppr a, ppr b]]
                      pure
                        $ mkCoreApps
                          (Core.Var $ exprIntCtor env)
                          [ Core.Type $ mkPromotedListTy liftedTypeKind $ fmap snd context
                          , mkCoreConApps intDataCon [Core.Lit $ LitNumber LitNumInt 999]
                          ]
          _ -> do
            liftMaybeT . putMsg $ hcat [text "unrecognised Int expr: ", ppr x]
            pure
              $ mkCoreApps
                (Core.Var $ exprIntCtor env)
                [ Core.Type $ mkPromotedListTy liftedTypeKind $ fmap snd context
                , mkCoreConApps intDataCon [Core.Lit $ LitNumber LitNumInt 999]
                ]
  -- boxed char literals
  quoteCoreExpr _localBinds context expr@(Core.App (Core.Var var) x)
    | var == dataConWorkId charDataCon =
        case x of
          Core.Lit LitChar{} ->
            pure
              $ mkCoreApps
                (Core.Var $ exprCharCtor env)
                [ Core.Type $ mkPromotedListTy liftedTypeKind $ fmap snd context
                , expr
                ]
          _ -> do
            liftMaybeT . putMsg $ hcat [text "unrecognised Char expr: ", ppr x]
            Control.Applicative.empty
  --  toString
  quoteCoreExpr _localBinds context expr@Core.App{}
    | (Core.Var f, [Core.Type ty, dict]) <- collectArgs expr
    , f == toStringVar env = do
        pure
          $ mkCoreApps
            (Core.Var $ exprToStringCtor env)
            [ Core.Type $ mkPromotedListTy liftedTypeKind $ fmap snd context
            , Core.Type ty
            , dict
            ]
  quoteCoreExpr localBinds context (Core.Var var) = do
    let u = unfoldingInfo $ idInfo var
    case expandUnfolding_maybe u of
      Nothing -> do
        liftMaybeT . warnMsg $ text "no unfolding for" <+> ppr var <+> parens (ppr u)
        (ty, index) <- mkIndex context var
        pure
          $ mkCoreApps
            (Core.Var $ exprVarCtor env)
            [ Core.Type $ mkPromotedListTy liftedTypeKind $ fmap snd context
            , Core.Type ty
            , index
            ]
      Just expr -> do
        liftMaybeT . putMsg $ hcat [text "inlining ", ppr var, text " to ", ppr expr]
        quoteCoreExpr localBinds context expr
  quoteCoreExpr localBinds context (Core.Lam var body) =
    if isVarOcc . occName $ varName var
      then do
        body' <- quoteCoreExpr localBinds ((var, varType var) : context) body
        pure
          $ mkCoreApps
            (Core.Var $ exprLamCtor env)
            [ Core.Type $ mkPromotedListTy liftedTypeKind $ fmap snd context
            , Core.Type $ varType var
            , Core.Type $ exprType body
            , body'
            ]
      else do
        body' <- quoteCoreExpr localBinds context body
        pure $ Core.Lam var body'
  quoteCoreExpr localBinds context expr@(Core.App f x) =
    if isValArg x
      then do
        -- liftMaybeT $ putMsgS "app case"
        f' <- quoteCoreExpr localBinds context f
        x' <- quoteCoreExpr localBinds context x
        pure
          $ mkCoreApps
            (Core.Var $ exprAppCtor env)
            [ Core.Type $ mkPromotedListTy liftedTypeKind $ fmap snd context
            , Core.Type $ exprType x
            , Core.Type $ exprType expr
            , f'
            , x'
            ]
      else do
        f' <- quoteCoreExpr localBinds context f
        pure $ Core.App f' x
  -- Pattern matching
  quoteCoreExpr localBinds context expr@(Core.Case val var ty branches)
    -- Int unboxing
    | isIntTy $ varType var
    , [Core.Alt (DataAlt ctor) [arg] body] <- branches
    , ctor == intDataCon = do
        quoteCoreExpr localBinds ((arg, intTy) : context) body
    -- Bool pattern matching
    | isBoolTy $ varType var
    , [Core.Alt (DataAlt ctor1) [] body1, Core.Alt DataAlt{} [] body2] <- branches = do
        val' <- quoteCoreExpr localBinds context val
        (trueCase, falseCase) <-
          if ctor1 == trueDataCon
            then (,) <$> quoteCoreExpr localBinds context body1 <*> quoteCoreExpr localBinds context body2
            else (,) <$> quoteCoreExpr localBinds context body2 <*> quoteCoreExpr localBinds context body1
        pure
          $ mkCoreApps
            (Core.Var $ exprIfThenElseCtor env)
            [ Core.Type $ mkPromotedListTy liftedTypeKind $ fmap snd context
            , Core.Type ty
            , val'
            , trueCase
            , falseCase
            ]
    -- Primops that return 0, 1 instead of true, false
    | varType var `eqType` intPrimTy
    , (Core.Var f, args) <- collectArgs val
    , f == primIntLt env
    , [left, right] <- args
    , [Core.Alt ctor1 [] body1, Core.Alt ctor2 [] body2] <- branches = do
        left' <- quoteCoreExpr localBinds context left
        right' <- quoteCoreExpr localBinds context right
        (trueCase, falseCase) <-
          case (ctor1, ctor2) of
            (DEFAULT, LitAlt (LitNumber _ 1)) ->
              (,) <$> quoteCoreExpr localBinds context body2 <*> quoteCoreExpr localBinds context body1
            (LitAlt (LitNumber _ 1), DEFAULT) ->
              (,) <$> quoteCoreExpr localBinds context body1 <*> quoteCoreExpr localBinds context body2
            _ -> do
              liftMaybeT . warnMsg $ text "unrecognised branching structure in " <+> ppr expr
              Control.Applicative.empty
        pure
          $ mkCoreApps
            (Core.Var $ exprIfThenElseCtor env)
            [ Core.Type $ mkPromotedListTy liftedTypeKind $ fmap snd context
            , Core.Type ty
            , mkCoreApps
                (Core.Var $ exprLtCtor env)
                [ Core.Type $ mkPromotedListTy liftedTypeKind $ fmap snd context
                , left'
                , right'
                ]
            , trueCase
            , falseCase
            ]
    -- Unpairing
    | [Alt (DataAlt dcon) [a, b] body] <- branches
    , dcon == tupleDataCon Boxed 2 = do
        let ctxTy = mkPromotedListTy liftedTypeKind $ fmap snd context
        let context' = (b, varType b) : (a, varType a) : context
        let ctxTy' = mkPromotedListTy liftedTypeKind $ fmap snd context'
        let aTy = exprType val
        let bTy = ty
        val' <- quoteCoreExpr localBinds context val
        body' <- quoteCoreExpr localBinds context' body
        let pat =
              mkCoreApps
                (Core.Var $ patternPPairCtor env)
                [ Core.Type ctxTy
                , Core.Type $ varType a
                , Core.Type $ varType b
                ]
        let branch =
              mkCoreApps
                (Core.Var $ branchBranchCtor env)
                [ Core.Type ctxTy
                , Core.Type ctxTy'
                , Core.Type aTy
                , Core.Type bTy
                , pat
                , body'
                ]
        pure
          $ mkCoreApps
            (Core.Var $ exprCaseCtor env)
            [ Core.Type ctxTy
            , Core.Type aTy
            , Core.Type bTy
            , val'
            , mkSingletonList (mkAppTys (mkTyConTy $ branchTyCon env) [ctxTy, aTy, bTy]) branch
            ]
    -- Unit elimination
    | [Alt (DataAlt dcon) [] body] <- branches
    , dcon == unitDataCon = do
        let ctxTy = mkPromotedListTy liftedTypeKind $ fmap snd context
        let aTy = exprType val
        let bTy = ty
        val' <- quoteCoreExpr localBinds context val
        body' <- quoteCoreExpr localBinds context body
        let pat =
              mkCoreApps
                (Core.Var $ patternPUnitCtor env)
                [ Core.Type ctxTy
                ]
        let branch =
              mkCoreApps
                (Core.Var $ branchBranchCtor env)
                [ Core.Type ctxTy
                , Core.Type ctxTy
                , Core.Type aTy
                , Core.Type bTy
                , pat
                , body'
                ]
        pure
          $ mkCoreApps
            (Core.Var $ exprCaseCtor env)
            [ Core.Type ctxTy
            , Core.Type aTy
            , Core.Type bTy
            , val'
            , mkSingletonList (mkAppTys (mkTyConTy $ branchTyCon env) [ctxTy, aTy, bTy]) branch
            ]
    -- Pattern matching on literals
    | varType var `eqType` intPrimTy = do
        val' <- quoteCoreExpr localBinds context val
        let ctxTy = mkPromotedListTy liftedTypeKind $ fmap snd context
        let aTy = intTy -- `Int#`s in core are quoted as `Expr ctx Int`s.
        let bTy = ty
        branches' <- quoteCoreBranches ctxTy aTy bTy localBinds context branches
        pure
          $ mkCoreApps
            (Core.Var $ exprCaseCtor env)
            [ Core.Type ctxTy
            , Core.Type aTy
            , Core.Type bTy
            , val'
            , branches'
            ]
   where
    quoteCoreBranches ::
      Type ->
      Type ->
      Type ->
      [CoreBind] ->
      [(CoreBndr, Type)] ->
      [CoreAlt] ->
      MaybeT CoreM CoreExpr
    quoteCoreBranches ctxTy aTy bTy _localBinds _context [] =
      pure $ mkCoreConApps nilDataCon [Core.Type $ mkAppTys (mkTyConTy $ branchTyCon env) [ctxTy, aTy, bTy]]
    quoteCoreBranches ctxTy aTy bTy localBinds context (alt@(Alt pattern args body) : alts) = do
      alt' <- case pattern of
        DEFAULT
          | [] <- args -> do
              let pdefault = mkCoreApps (Core.Var $ patternPDefaultCtor env) [Core.Type ctxTy, Core.Type aTy]
              body' <- quoteCoreExpr localBinds context body
              pure
                $ mkCoreApps
                  (Core.Var $ branchBranchCtor env)
                  [ Core.Type ctxTy
                  , Core.Type ctxTy
                  , Core.Type aTy
                  , Core.Type bTy
                  , pdefault
                  , body'
                  ]
        LitAlt (LitNumber LitNumInt i) | [] <- args -> do
          let pint = mkCoreApps (Core.Var $ patternPIntCtor env) [Core.Type ctxTy, Core.Lit $ mkLitIntUnchecked i]
          body' <- quoteCoreExpr localBinds context body
          pure
            $ mkCoreApps
              (Core.Var $ branchBranchCtor env)
              [ Core.Type ctxTy
              , Core.Type ctxTy
              , Core.Type aTy
              , Core.Type bTy
              , pint
              , body'
              ]
        _ -> do
          liftMaybeT . warnMsg $ text "unrecognised pattern branch: " <+> ppr alt
          Control.Applicative.empty
      alts' <- quoteCoreBranches ctxTy aTy bTy localBinds context alts
      pure $ mkCoreConApps consDataCon [Core.Type $ mkAppTys (mkTyConTy $ branchTyCon env) [ctxTy, aTy, bTy], alt', alts']
  quoteCoreExpr _localBinds context expr = do
    liftMaybeT $ do
      namePprCtx <- getNamePprCtx
      dflags <- getDynFlags
      putMsg
        . pprLocMsgEnvelopeDefault
        $ mkMsgEnvelope (initDiagOpts dflags) (UnhelpfulSpan UnhelpfulNoLocationInfo) namePprCtx
        $ GhcUnknownMessage
        $ UnknownDiagnostic
          DiagnosticMessage
            { diagMessage = mkSimpleDecorated $ text "unrecognised expression: " <+> ppr expr
            , diagReason = WarningWithoutFlag
            , diagHints = []
            }
    pure
      $ mkCoreApps
        (Core.Var $ exprIntCtor env)
        [ Core.Type $ mkPromotedListTy liftedTypeKind $ fmap snd context
        , mkCoreConApps intDataCon [Core.Lit $ LitNumber LitNumInt 999]
        ]

warnMsg :: SDoc -> CoreM ()
warnMsg sdoc = do
  namePprCtx <- getNamePprCtx
  dflags <- getDynFlags
  putMsg
    . pprLocMsgEnvelopeDefault
    $ mkMsgEnvelope (initDiagOpts dflags) (UnhelpfulSpan UnhelpfulNoLocationInfo) namePprCtx
    $ GhcUnknownMessage
    $ UnknownDiagnostic
      DiagnosticMessage
        { diagMessage = mkSimpleDecorated sdoc
        , diagReason = WarningWithoutFlag
        , diagHints = []
        }

mkSingletonList :: Type -> CoreExpr -> CoreExpr
mkSingletonList ty el = mkCoreConApps consDataCon [Core.Type ty, el, mkCoreConApps nilDataCon [Core.Type ty]]