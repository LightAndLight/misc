{-# LANGUAGE LambdaCase #-}

module GHC.Lens where

import Control.Lens
import GHC
  ( Anchor
  , AnchorOperation (..)
  , ConDecl (..)
  , ConDeclField
  , DataDefnCons (..)
  , DeltaPos
  , EpAnn (..)
  , EpAnnComments
  , EpaLocation (..)
  , GRHS (..)
  , GRHSs
  , GenLocated (..)
  , GhcPs
  , GuardLStmt
  , HsBind
  , HsBindLR (..)
  , HsConDeclH98Details
  , HsConDetails (..)
  , HsDataDefn
  , HsDecl (..)
  , HsModule
  , LBangType
  , LConDecl
  , LEpaComment
  , LGRHS
  , LHsContext
  , LHsDecl
  , LHsDoc
  , LHsExpr
  , LHsQTyVars
  , LHsTyVarBndr
  , LIdP
  , LMatch
  , LexicalFixity
  , Match
  , MatchGroup
  , SrcSpanAnn'
  , TyClDecl (..)
  , XCGRHS
  , XConDeclH98
  , XDataDecl
  , XFunBind
  , XRec
  , XTyClD
  , XValD
  )
import qualified GHC
import GHC.Types.Var (Specificity)

_EpaDelta :: Prism' EpaLocation (DeltaPos, [LEpaComment])
_EpaDelta = prism' (uncurry EpaDelta) (\case EpaDelta a b -> Just (a, b); _ -> Nothing)

_MovedAnchor :: Prism' AnchorOperation DeltaPos
_MovedAnchor = prism' MovedAnchor (\case MovedAnchor a -> Just a; _ -> Nothing)

anchor_op :: Lens' Anchor AnchorOperation
anchor_op = lens GHC.anchor_op (\x a -> x{GHC.anchor_op = a})

data EpAnnArgs ann = EpAnnArgs
  { _epAnn_entry :: !Anchor
  , _epAnn_anns :: !ann
  , _epAnn_comments :: !EpAnnComments
  }

epAnn_entry :: Lens' (EpAnnArgs ann) Anchor
epAnn_entry = lens _epAnn_entry (\x a -> x{_epAnn_entry = a})

epAnn_anns :: Lens (EpAnnArgs ann) (EpAnnArgs ann') ann ann'
epAnn_anns = lens _epAnn_anns (\x a -> x{_epAnn_anns = a})

_EpAnn :: Prism (EpAnn a) (EpAnn b) (EpAnnArgs a) (EpAnnArgs b)
_EpAnn =
  prism
    (\(EpAnnArgs a b c) -> EpAnn a b c)
    (\case EpAnn a b c -> Right (EpAnnArgs a b c); EpAnnNotUsed -> Left EpAnnNotUsed)

ann :: Lens (SrcSpanAnn' a) (SrcSpanAnn' b) a b
ann = lens GHC.ann (\x a -> x{GHC.ann = a})

cd_fld_type :: Lens' (ConDeclField pass) (LBangType pass)
cd_fld_type = lens GHC.cd_fld_type (\x a -> x{GHC.cd_fld_type = a})

data ConDeclH98Args pass = ConDeclH98Args
  { _con_ext :: XConDeclH98 pass
  , _con_name :: LIdP pass
  , _con_forall :: Bool
  , _con_ex_tvs :: [LHsTyVarBndr Specificity pass]
  , _con_mb_cxt :: Maybe (LHsContext pass)
  , _con_args :: HsConDeclH98Details pass
  , _con_doc :: Maybe (LHsDoc pass)
  }

_ConDeclH98 :: Prism' (ConDecl pass) (ConDeclH98Args pass)
_ConDeclH98 =
  prism'
    (\(ConDeclH98Args a b c d e f g) -> ConDeclH98 a b c d e f g)
    ( \case
        ConDeclH98 a b c d e f g -> Just (ConDeclH98Args a b c d e f g)
        _ -> Nothing
    )

con_args :: Lens' (ConDeclH98Args pass) (HsConDeclH98Details pass)
con_args = lens _con_args (\x a -> x{_con_args = a})

_RecCon ::
  Prism
    (HsConDetails tyarg arg rec)
    (HsConDetails tyarg arg rec')
    rec
    rec'
_RecCon =
  prism
    RecCon
    ( \case
        PrefixCon a b -> Left (PrefixCon a b)
        RecCon a -> Right a
        InfixCon a b -> Left (InfixCon a b)
    )

_DataTypeCons_data :: Prism' (DataDefnCons a) [a]
_DataTypeCons_data =
  prism' (DataTypeCons False) (\case DataTypeCons False a -> Just a; _ -> Nothing)

dd_cons :: Lens' (HsDataDefn pass) (DataDefnCons (LConDecl pass))
dd_cons = lens GHC.dd_cons (\x a -> x{GHC.dd_cons = a})

l_loc :: Lens (GenLocated l a) (GenLocated l' a) l l'
l_loc = lens (\(L l _) -> l) (\(L _ a) l' -> L l' a)

l_val :: Lens (GenLocated l a) (GenLocated l b) a b
l_val = lens (\(L _ a) -> a) (\(L l _) b -> L l b)

hsmodDecls :: Lens' (HsModule GhcPs) [LHsDecl GhcPs]
hsmodDecls = lens GHC.hsmodDecls (\x a -> x{GHC.hsmodDecls = a})

data ValDArgs p = ValDArgs {_valD_ext :: XValD p, _valD_bind :: HsBind p}

valD_bind :: Lens' (ValDArgs p) (HsBind p)
valD_bind = lens _valD_bind (\x a -> x{_valD_bind = a})

_ValD :: Prism' (HsDecl p) (ValDArgs p)
_ValD = prism' (\(ValDArgs a b) -> ValD a b) (\case ValD a b -> Just $ ValDArgs a b; _ -> Nothing)

_TyClD :: Prism' (HsDecl p) (XTyClD p, TyClDecl p)
_TyClD = prism' (uncurry TyClD) (\case TyClD a b -> Just (a, b); _ -> Nothing)

_DataDecl ::
  Prism' (TyClDecl pass) (XDataDecl pass, LIdP pass, LHsQTyVars pass, LexicalFixity, HsDataDefn pass)
_DataDecl =
  prism'
    (\(a, b, c, d, e) -> DataDecl a b c d e)
    (\case DataDecl a b c d e -> Just (a, b, c, d, e); _ -> Nothing)

data FunBindArgs idL idR = FunBindArgs
  { _fun_ext :: XFunBind idL idR
  , _fun_id :: LIdP idL
  , _fun_matches :: MatchGroup idR (LHsExpr idR)
  }

fun_id :: Lens' (FunBindArgs idL idR) (LIdP idL)
fun_id = lens _fun_id (\x a -> x{_fun_id = a})

fun_matches :: Lens' (FunBindArgs idL idR) (MatchGroup idR (LHsExpr idR))
fun_matches = lens _fun_matches (\x a -> x{_fun_matches = a})

_FunBind :: Prism' (HsBindLR idL idR) (FunBindArgs idL idR)
_FunBind =
  prism'
    (\(FunBindArgs a b c) -> FunBind a b c)
    (\case FunBind a b c -> Just (FunBindArgs a b c); _ -> Nothing)

mg_alts :: Lens' (MatchGroup p body) (XRec p [LMatch p body])
mg_alts = lens GHC.mg_alts (\x a -> x{GHC.mg_alts = a})

m_grhss :: Lens' (Match p body) (GRHSs p body)
m_grhss = lens GHC.m_grhss (\x a -> x{GHC.m_grhss = a})

grhssGRHSs :: Lens' (GRHSs p body) [LGRHS p body]
grhssGRHSs = lens GHC.grhssGRHSs (\x a -> x{GHC.grhssGRHSs = a})

data GRHSArgs p body = GRHSArgs
  { _grhs_ext :: XCGRHS p body
  , _grhs_guards :: [GuardLStmt p]
  , _grhs_body :: body
  }

grhs_body :: Lens' (GRHSArgs p body) body
grhs_body = lens _grhs_body (\x a -> x{_grhs_body = a})

_GRHS :: Prism' (GRHS p body) (GRHSArgs p body)
_GRHS =
  prism' (\(GRHSArgs a b c) -> GRHS a b c) (\case GRHS a b c -> Just (GRHSArgs a b c); _ -> Nothing)
