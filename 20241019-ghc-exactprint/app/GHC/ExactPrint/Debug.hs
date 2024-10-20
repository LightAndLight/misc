{-# LANGUAGE OverloadedStrings #-}

module GHC.ExactPrint.Debug where

import Data.Bool (bool)
import Data.Void (absurd)
import GHC
import GHC.Utils.Outputable hiding ((<>))
import qualified GHC.Utils.Outputable as SDoc

sexpr :: Foldable t => SDoc -> t SDoc -> SDoc
sexpr name contents =
  text "(" SDoc.<> name $+$ nest 2 (foldr ($+$) empty contents SDoc.<> text ")")

todo :: Outputable a => a -> SDoc
todo x = text "TODO: " <+> ppr x

debugHsDecl :: HsDecl GhcPs -> SDoc
debugHsDecl decl =
  case decl of
    TyClD NoExtField decl' ->
      sexpr
        "TyClD"
        [ debugTyClDecl decl'
        ]
    _ -> text "TODO: " <+> ppr decl

debugTyClDecl :: TyClDecl GhcPs -> SDoc
debugTyClDecl decl =
  case decl of
    DataDecl ext tyCon tyVars _fixity dataDefn ->
      sexpr
        "DataDecl"
        [ debugEpAnn (debugList debugAddEpAnn) ext
        , debugVar tyCon
        , debugLHsQTyVars tyVars
        , debugHsDataDefn dataDefn
        ]
    _ ->
      text "TODO: " <+> ppr decl

debugHsDataDefn :: HsDataDefn GhcPs -> SDoc
debugHsDataDefn (HsDataDefn NoExtField ctxt cType kindSig cons derivs) =
  sexpr
    "HsDataDefn"
    [ debugMaybe ((text "TODO: " <+>) . ppr) ctxt
    , debugMaybe ((text "TODO: " <+>) . ppr) cType
    , debugMaybe ((text "TODO: " <+>) . ppr) kindSig
    , debugDataDefnCons cons
    , text "TODO: " <+> ppr derivs
    ]

debugDataDefnCons :: DataDefnCons (LConDecl GhcPs) -> SDoc
debugDataDefnCons defn =
  case defn of
    DataTypeCons False a ->
      sexpr
        "DataTypeCons_data"
        [ debugList (debugLocated debugSrcSpanAnnA debugConDecl) a
        ]
    DataTypeCons True a ->
      sexpr
        "DataTypeCons_type"
        [ debugList (debugLocated debugSrcSpanAnnA debugConDecl) a
        ]
    NewTypeCon a ->
      sexpr
        "NewTypeCon"
        [ debugLocated debugSrcSpanAnnA debugConDecl a
        ]

debugConDecl :: ConDecl GhcPs -> SDoc
debugConDecl decl =
  case decl of
    ConDeclH98 ext name fa ex_tvs mb_cxt args doc ->
      sexpr
        "ConDeclH98"
        [ debugEpAnn (debugList debugAddEpAnn) ext
        , debugVar name
        , bool (text "False") (text "True") fa
        , debugList (debugLocated debugSrcSpanAnnA ((text "TODO: " <+>) . ppr)) ex_tvs
        , debugMaybe ((text "TODO" <+>) . ppr) mb_cxt
        , debugHsConDeclH98Details args
        , debugMaybe ((text "TODO: " <+>) . ppr) doc
        ]
    _ ->
      text "TODO: " <+> ppr decl
debugHsConDeclH98Details :: HsConDeclH98Details GhcPs -> SDoc
debugHsConDeclH98Details details =
  case details of
    PrefixCon (x : _) _ ->
      absurd x
    PrefixCon [] b ->
      sexpr
        "PrefixCon"
        [ debugList debugHsScaled b
        ]
    RecCon a ->
      sexpr
        "RecCon"
        [ debugLocated debugSrcSpanAnnL (debugList $ debugLocated debugSrcSpanAnnA debugConDeclField) a
        ]
    InfixCon a b ->
      sexpr
        "InfixCon"
        [ debugHsScaled a
        , debugHsScaled b
        ]

debugHsScaled :: HsScaled GhcPs (LBangType GhcPs) -> SDoc
debugHsScaled (HsScaled a b) =
  sexpr
    "HsScaled"
    [ todo a
    , todo b
    ]

debugLHsQTyVars :: LHsQTyVars GhcPs -> SDoc
debugLHsQTyVars (HsQTvs NoExtField explicit) =
  sexpr
    "HsQTvs"
    [ debugList (debugLocated debugSrcSpanAnnA $ (text "TODO: " <+>) . ppr) explicit
    ]

debugConDeclField :: ConDeclField GhcPs -> SDoc
debugConDeclField (ConDeclField ext names ty doc) =
  sexpr
    "ConDeclField"
    [ debugEpAnn (debugList debugAddEpAnn) ext
    , debugList (debugLocated debugSrcAnnNoEpAnns debugFieldOcc) names
    , debugLocated debugSrcSpanAnnA todo ty
    , debugMaybe todo doc
    ]

debugFieldOcc :: FieldOcc GhcPs -> SDoc
debugFieldOcc fieldOcc =
  case fieldOcc of
    FieldOcc NoExtField label -> debugVar label

debugSrcAnnNoEpAnns :: SrcAnn NoEpAnns -> SDoc
debugSrcAnnNoEpAnns (SrcSpanAnn x _) = debugEpAnn (\NoEpAnns -> text "NoEpAnns") x

debugLocated :: (l -> SDoc) -> (a -> SDoc) -> GenLocated l a -> SDoc
debugLocated debugL debugA (L l a) =
  sexpr "L" [debugL l, debugA a]

debugEpAnn :: (a -> SDoc) -> EpAnn a -> SDoc
debugEpAnn _ EpAnnNotUsed = text "EpAnnNotUsed"
debugEpAnn f (EpAnn anc a _comments) =
  sexpr "EpAnn" [debugAnchor anc, f a]

debugAnchor :: Anchor -> SDoc
debugAnchor anc =
  case GHC.anchor_op anc of
    UnchangedAnchor ->
      parens (text "UnchangedAnchor" <+> ppr (anchor anc))
    MovedAnchor delta ->
      parens (text "MovedAnchor" <+> doubleQuotes (ppr $ anchor anc) <+> debugDeltaPos delta)

debugDeltaPos :: DeltaPos -> SDoc
debugDeltaPos delta =
  parens (text "delta" <+> ppr (getDeltaLine delta) <+> ppr (deltaColumn delta))

debugEpAnnsLHsExpr :: LHsExpr GhcPs -> SDoc
debugEpAnnsLHsExpr = debugLocated debugSrcSpanAnnA debugEpAnnsHsExpr

debugSrcSpanAnnA :: SrcSpanAnnA -> SDoc
debugSrcSpanAnnA (SrcSpanAnn epAnn _) = debugEpAnn debugAnnListItem epAnn

debugMaybe :: (a -> SDoc) -> Maybe a -> SDoc
debugMaybe _ Nothing = text "Nothing"
debugMaybe f (Just a) = sexpr "Just" [f a]

debugAddEpAnn :: AddEpAnn -> SDoc
debugAddEpAnn (AddEpAnn token location) =
  sexpr
    "AddEpAnn"
    [ ppr token
    , debugEpaLocation location
    ]

debugList :: (a -> SDoc) -> [a] -> SDoc
debugList _ [] = empty
debugList f (x : xs) = f x $+$ debugList f xs

debugAnnList :: AnnList -> SDoc
debugAnnList (AnnList anc open close rest trailing) =
  sexpr
    "AnnList"
    [ debugMaybe debugAnchor anc
    , debugMaybe debugAddEpAnn open
    , debugMaybe debugAddEpAnn close
    , debugList debugAddEpAnn rest
    , debugList debugTrailingAnn trailing
    ]

debugSrcSpanAnnL :: SrcSpanAnnL -> SDoc
debugSrcSpanAnnL (SrcSpanAnn epAnn _) = debugEpAnn debugAnnList epAnn

debugAnnListItem :: AnnListItem -> SDoc
debugAnnListItem (AnnListItem items) =
  foldr (($+$) . debugTrailingAnn) empty items

debugTrailingAnn :: TrailingAnn -> SDoc
debugTrailingAnn ta =
  parens
    ( text "TrailingAnn"
        <+> case ta of
          AddSemiAnn loc -> text "';'" <+> debugEpaLocation loc
          AddCommaAnn loc -> text "','" <+> debugEpaLocation loc
          AddVbarAnn loc -> text "'|'" <+> debugEpaLocation loc
    )

debugEpaLocation :: EpaLocation -> SDoc
debugEpaLocation location =
  case location of
    EpaSpan rss _ ->
      parens (text "EpaSpan" <+> doubleQuotes (ppr rss))
    EpaDelta delta _comments ->
      parens (text "EpaDelta" <+> debugDeltaPos delta)

debugVar :: LocatedN RdrName -> SDoc
debugVar (L (SrcSpanAnn epAnn _) a) =
  sexpr
    "RdrName"
    [ debugEpAnn (parens . ppr) epAnn
    , doubleQuotes $ ppr a
    ]

debugEpAnnsHsExpr :: HsExpr GhcPs -> SDoc
debugEpAnnsHsExpr expr =
  case expr of
    RecordCon ext con fields ->
      sexpr
        "RecordCon"
        [ debugEpAnn (debugList debugAddEpAnn) ext
        , debugVar con
        , debugEpAnnsHsRecFields fields
        ]
    HsApp ext f x ->
      sexpr
        "HsApp"
        [ debugEpAnn (\NoEpAnns -> text "NoEpAnns") ext
        , debugLocated debugSrcSpanAnnA debugEpAnnsHsExpr f
        , debugLocated debugSrcSpanAnnA debugEpAnnsHsExpr x
        ]
    HsVar NoExtField x ->
      sexpr
        "HsVar"
        [ debugVar x
        ]
    OpApp ext a b c ->
      sexpr
        "OpApp"
        [ debugEpAnn (debugList debugAddEpAnn) ext
        , debugLocated debugSrcSpanAnnA debugEpAnnsHsExpr a
        , debugLocated debugSrcSpanAnnA debugEpAnnsHsExpr b
        , debugLocated debugSrcSpanAnnA debugEpAnnsHsExpr c
        ]
    HsDo ext _doFlavour b ->
      sexpr
        "HsDo"
        [ debugEpAnn debugAnnList ext
        , debugLocated debugSrcSpanAnnL (debugList (debugLocated debugSrcSpanAnnA debugStmt)) b
        ]
    _ -> text "TODO:" <+> ppr expr
  where
    debugStmt :: Stmt GhcPs (LHsExpr GhcPs) -> SDoc
    debugStmt stmt =
      case stmt of
        BodyStmt NoExtField body NoExtField NoExtField ->
          sexpr
            "BodyStmt"
            [ debugLocated debugSrcSpanAnnA debugEpAnnsHsExpr body
            ]
        _ ->
          text "TODO:" <+> ppr stmt

    debugEpAnnsHsRecFields :: HsRecordBinds GhcPs -> SDoc
    debugEpAnnsHsRecFields (HsRecFields fields _dotdot) =
      sexpr "HsRecFields" $
        fmap (debugLocated debugSrcSpanAnnA debugEpAnnsHsRecField) fields

    debugSrcSpanNoEpAnns :: SrcAnn NoEpAnns -> SDoc
    debugSrcSpanNoEpAnns (SrcSpanAnn x _) = debugEpAnn (\NoEpAnns -> text "NoEpAnns") x

    debugEpAnnsHsRecField :: HsRecField GhcPs (LHsExpr GhcPs) -> SDoc
    debugEpAnnsHsRecField (HsFieldBind epAnn lhs rhs _pun) =
      sexpr
        "HsFieldBind"
        [ debugEpAnn (debugList debugAddEpAnn) epAnn
        , debugLocated debugSrcSpanNoEpAnns debugFieldOcc lhs
        , debugLocated debugSrcSpanAnnA debugEpAnnsHsExpr rhs
        ]
