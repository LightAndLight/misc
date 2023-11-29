{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Compiler.Plugin (plugin) where

import Data.Maybe (fromJust)
import Data.Typeable (cast)
import qualified GHC.Core as Core
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
        boxIntCtor <- findCtor "GHC.Int" "I#"
        let env =
              Env
                { quoteVar
                , quotedCtor
                , undefinedVar
                , exprIntCtor
                , boxIntCtor
                }
        pure $ CoreDoPluginPass "compiler-plugin-pass" (pass env) : todos
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

pass :: Env -> CorePluginPass
pass env = bindsOnlyPass (traverse bindPass)
 where
  bindPass :: CoreBind -> CoreM CoreBind
  bindPass =
    everywhereM $ \(term :: a) ->
      case cast @a @CoreExpr term of
        Just expr ->
          fromJust . cast @CoreExpr @a <$> exprPass expr
        Nothing ->
          pure term

  exprPass :: CoreExpr -> CoreM CoreExpr
  exprPass expr@(Core.collectArgs -> (Core.Var var, args)) = do
    let name = varName var
    case filter (\case Type _ -> False; _ -> True) args of
      [callStack, arg] ->
        if and
          [ fmap (moduleNameString . moduleName) (nameModule_maybe name) == Just "Compiler.Plugin.Interface"
          , var == quoteVar env -- occNameString (nameOccName name) == "quote"
          ]
          then do
            putMsg $ hcat [ppr name, text " applied to ", ppr callStack, text " and ", ppr arg]
            let expr' = quotedCore arg
            putMsg $ ppr expr'
            pure expr'
          else pure expr
      _ ->
        pure expr
  exprPass expr = pure expr

  quotedCore :: CoreExpr -> CoreExpr
  quotedCore expr =
    Core.App
      ( Core.App
          ( Core.App
              (Core.Var $ quotedCtor env)
              (Core.Type (exprType expr))
          )
          (quoteCoreExpr [] expr)
      )
      expr

  quoteCoreExpr :: [Type] -> CoreExpr -> CoreExpr
  quoteCoreExpr context expr =
    -- TODO: the real work
    Core.App
      ( Core.App
          (Core.Var $ exprIntCtor env)
          (Core.Type $ mkPromotedListTy liftedTypeKind context)
      )
      (Core.App (Core.Var $ boxIntCtor env) (Core.Lit $ LitNumber LitNumInt 999))