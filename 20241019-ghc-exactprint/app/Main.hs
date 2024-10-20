{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Lens hiding (cons)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import GHC.Driver.Config.Diagnostic (initDiagOpts, initPrintConfig)
import GHC.Driver.Errors (printMessages)
import GHC.Paths (libdir)
import GHC.Utils.Logger (getLogger)
import Language.Haskell.GHC.ExactPrint (exactPrint, makeDeltaAst)
import Language.Haskell.GHC.ExactPrint.Parsers
  ( defaultCppOptions
  , ghcWrapper
  , initDynFlags
  , parseModuleEpAnnsWithCppInternal
  , postParseTransform
  )

import GHC.ExactPrint.Transform (addFieldToRecord, addFieldToRecordDataDefn)
import GHC.Lens
import GHC.Syntax (integer, rdrNameString, tyCon)

main :: IO ()
main = do
  let path = "data/File.hs"
  ghcWrapper libdir $ do
    dflags <- initDynFlags path
    res <- parseModuleEpAnnsWithCppInternal defaultCppOptions dflags path
    case postParseTransform res of
      Left errs -> do
        logger <- getLogger
        liftIO $ printMessages logger (initPrintConfig dflags) (initDiagOpts dflags) errs
      Right (makeDeltaAst -> parsed) -> do
        let
          modified =
            over
              (l_val . hsmodDecls . mapped)
              (\decl -> fromMaybe decl $ addFieldToRecordDataDefn "X" ("d", tyCon "Int") decl)
              $ over
                ( l_val
                    . hsmodDecls
                    . mapped
                    . l_val
                    . _ValD
                    . valD_bind
                )
                ( \valD -> fromMaybe valD $ do
                    funBind <- valD ^? _FunBind
                    guard $ "rewrite" `isPrefixOf` rdrNameString (funBind ^. fun_id . l_val)
                    pure $
                      review _FunBind $
                        over
                          ( fun_matches
                              . mg_alts
                              . l_val
                              . mapped
                              . l_val
                              . m_grhss
                              . grhssGRHSs
                              . mapped
                              . l_val
                              . _GRHS
                              . grhs_body
                          )
                          (\expr -> fromMaybe expr $ addFieldToRecord ("d", integer 4) expr)
                          funBind
                )
                parsed

        liftIO $ writeFile (path <> ".new") (exactPrint modified)
