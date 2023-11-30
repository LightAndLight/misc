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
import qualified GHC.Core as Core
import GHC.Data.Maybe (MaybeT, fromMaybe, liftMaybeT, runMaybeT)
import GHC.Plugins hiding (Expr)
import GHC.Runtime.Loader (lookupRdrNameInModuleForPlugins)
import GHC.Types.TyThing (lookupId)
import Generics.SYB (everywhereM)
import Prelude hiding (mod)

data Env = Env
  { quoteVar :: Id
  , quotedCtor :: Id
  , undefinedVar :: Id
  , exprIntCtor :: Id
  , exprLamCtor :: Id
  , exprAppCtor :: Id
  , exprVarCtor :: Id
  , indexZCtor :: Id
  , indexSCtor :: Id
  , boxIntCtor :: Id
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
        exprIntCtor <- findCtor "Compiler.Plugin.Interface" "Int"
        exprLamCtor <- findCtor "Compiler.Plugin.Interface" "Lam"
        exprAppCtor <- findCtor "Compiler.Plugin.Interface" "App"
        exprVarCtor <- findCtor "Compiler.Plugin.Interface" "Var"
        indexZCtor <- findCtor "Compiler.Plugin.Interface" "Z"
        indexSCtor <- findCtor "Compiler.Plugin.Interface" "S"
        boxIntCtor <- findCtor "GHC.Int" "I#"
        let env =
              Env
                { quoteVar
                , quotedCtor
                , undefinedVar
                , exprIntCtor
                , exprLamCtor
                , exprAppCtor
                , exprVarCtor
                , indexZCtor
                , indexSCtor
                , boxIntCtor
                }
        -- putMsg $ ppr todos
        pure $ todos ++ [CoreDoPluginPass "compiler-plugin-pass" (pass env)]
    }

-- Reference: https://github.com/compiling-to-categories/concat/blob/5d670b96cd770c6b96f55429bd3a830322ffaf16/inline/src/ConCat/Inline/Plugin.hs#L56
findCtor :: String -> String -> CoreM Id
findCtor mod str = do
  hsc_env <- getHscEnv
  mInfo <- liftIO (lookupRdrNameInModuleForPlugins hsc_env (mkModuleName mod) (Unqual (mkDataOcc str)))
  maybe (panic err) (lookupId . fst) mInfo
 where
  err = "findId: couldn't find " ++ str ++ " in " ++ moduleNameString (mkModuleName mod)

findVar :: String -> String -> CoreM Id
findVar mod str = do
  hsc_env <- getHscEnv
  mInfo <- liftIO (lookupRdrNameInModuleForPlugins hsc_env (mkModuleName mod) (Unqual (mkVarOcc str)))
  maybe (panic err) (lookupId . fst) mInfo
 where
  err = "findId: couldn't find " ++ str ++ " in " ++ moduleNameString (mkModuleName mod)

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
  quoteCoreExpr localBinds context (Core.Var var) =
    case lookupLocalBind localBinds var of
      Nothing -> do
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
   where
    lookupLocalBind :: [CoreBind] -> CoreBndr -> Maybe CoreExpr
    lookupLocalBind [] _ = Nothing
    lookupLocalBind (NonRec name value : rest) bndr =
      if name == bndr then Just value else lookupLocalBind rest bndr
    lookupLocalBind (Rec _ : rest) bndr =
      -- TODO: handle inlining of recursive functions
      lookupLocalBind rest bndr
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
  quoteCoreExpr _localBinds context _expr =
    -- TODO: the real work
    pure
      $ mkCoreApps
        (Core.Var $ exprIntCtor env)
        [ Core.Type $ mkPromotedListTy liftedTypeKind $ fmap snd context
        , Core.App (Core.Var $ boxIntCtor env) (Core.Lit $ LitNumber LitNumInt 999)
        ]