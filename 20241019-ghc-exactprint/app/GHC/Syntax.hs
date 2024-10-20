module GHC.Syntax where

import GHC
import GHC.Types.Name.Occurrence (mkTcOcc, mkVarOcc, occNameString)
import GHC.Types.Name.Reader (mkRdrUnqual, rdrNameOcc)
import GHC.Types.SourceText (IntegralLit (..), SourceText (..))
import GHC.Types.SrcLoc (generatedSrcSpan)

generatedAnchor :: Anchor
generatedAnchor = spanAsAnchor generatedSrcSpan

tyConName :: String -> RdrName
tyConName = mkRdrUnqual . mkTcOcc

tyCon :: String -> HsType GhcPs
tyCon = HsTyVar noAnn NotPromoted . L noSrcSpanA . tyConName

integer :: Integer -> HsExpr GhcPs
integer i =
  HsOverLit noComments
    . OverLit NoExtField
    . HsIntegral
    $ IL NoSourceText (i < 0) (abs i)

varName :: String -> RdrName
varName = mkRdrUnqual . mkVarOcc

rdrNameString :: RdrName -> String
rdrNameString = occNameString . rdrNameOcc

var :: String -> HsExpr GhcPs
var = HsVar noExtField . L noSrcSpanA . varName

app :: HsExpr GhcPs -> HsExpr GhcPs -> HsExpr GhcPs
app f x =
  HsApp
    noComments
    ( L
        ( SrcSpanAnn
            ( EpAnn
                {-
                  f x
                  ^
                -}
                generatedAnchor
                {-
                  f x
                   ^
                -}
                (AnnListItem [])
                emptyComments
            )
            generatedSrcSpan
        )
        f
    )
    ( L
        ( SrcSpanAnn
            ( EpAnn
                {-
                f x
                  ^
                -}
                generatedAnchor{GHC.anchor_op = MovedAnchor $ deltaPos 0 1}
                {-
                f x
                   ^
                -}
                (AnnListItem [])
                emptyComments
            )
            generatedSrcSpan
        )
        x
    )
