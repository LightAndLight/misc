{-# LANGUAGE LambdaCase #-}

module GHC.ExactPrint.Transform where

import Control.Lens.Cons (_head, _last)
import Control.Lens.Fold ((^?))
import Control.Lens.Getter ((^.))
import Control.Lens.Setter ((.~))
import Control.Lens.Tuple (_1, _2, _5)
import Control.Monad (guard)
import Data.Function ((&))
import Data.Maybe (fromMaybe, mapMaybe)
import GHC
  ( AddEpAnn (..)
  , AnchorOperation (..)
  , AnnKeywordId (..)
  , AnnListItem (..)
  , BangType
  , ConDeclField (ConDeclField)
  , DeltaPos
  , EpAnn (EpAnn, EpAnnNotUsed)
  , EpaLocation (..)
  , FieldOcc (..)
  , GenLocated (..)
  , GhcPs
  , HsExpr (..)
  , HsFieldBind (..)
  , HsRecFields (..)
  , LHsDecl
  , LHsExpr
  , LHsRecField
  , NoExtField (..)
  , SrcSpanAnn' (SrcSpanAnn)
  , SrcSpanAnnA
  , TrailingAnn (..)
  , addTrailingAnnToA
  , deltaPos
  , emptyComments
  , getLoc
  , mkFieldOcc
  , noSrcSpanA
  )
import qualified GHC
import GHC.Types.Name.Occurrence (mkVarOcc)
import GHC.Types.Name.Reader (mkRdrUnqual)
import GHC.Types.SrcLoc (generatedSrcSpan)

import GHC.Lens
import GHC.Syntax (generatedAnchor, tyConName)

addFieldToRecord :: (String, HsExpr GhcPs) -> LHsExpr GhcPs -> Maybe (LHsExpr GhcPs)
addFieldToRecord (fieldName, fieldValue) (L loc (RecordCon ex con (HsRecFields fields Nothing))) =
  Just $
    L
      loc
      ( RecordCon
          ex
          con
          ( HsRecFields
              ( case fields ^? _last of
                  Nothing ->
                    {- The record has no fields.

                      * Put the field root one space after the `{`.
                      * Put the field RHS one space after the `=`.
                    -}
                    [newField (deltaPos 0 1) (deltaPos 0 1)]
                  Just lastField ->
                    {- The record has fields.

                      * Place a comma at the end of the current fields
                        * Use the same relative position as previous commas
                        * Fall back to the location of `{`, then `deltaPos 0 0`
                      * Reuse the record field root and RHS deltas
                    -}
                    addTrailingComma (fromMaybe (deltaPos 0 0) $ openCDelta ex) fields
                      ++ [newField (recFieldDelta lastField) (recFieldRhsDelta lastField)]
              )
              Nothing
          )
      )
  where
    newField :: DeltaPos -> DeltaPos -> LHsRecField GhcPs (LHsExpr GhcPs)
    newField newFieldDelta newFieldRhsDelta =
      L
        ( SrcSpanAnn
            ( EpAnn
                {-
                field = rhs
                \^
                -}
                generatedAnchor{GHC.anchor_op = MovedAnchor newFieldDelta}
                {-
                field = rhs
                           ^
                -}
                (AnnListItem [])
                emptyComments
            )
            generatedSrcSpan
        )
        HsFieldBind
          { hfbAnn =
              EpAnn
                {-
                field = rhs
                \^
                -}
                generatedAnchor
                {-
                field = rhs
                      ^

                It seems that only `AnnEqual` is accepted here.
                -}
                [AddEpAnn AnnEqual $ EpaDelta (deltaPos 0 1) []]
                emptyComments
          , hfbLHS =
              L noSrcSpanA . mkFieldOcc . L noSrcSpanA . mkRdrUnqual $ mkVarOcc fieldName
          , hfbRHS =
              L
                ( SrcSpanAnn
                    ( EpAnn
                        {-
                        field = rhs
                                ^
                        -}
                        generatedAnchor{GHC.anchor_op = MovedAnchor newFieldRhsDelta}
                        {-
                        field = rhs
                                   ^
                        -}
                        (AnnListItem [])
                        emptyComments
                    )
                    generatedSrcSpan
                )
                fieldValue
          , hfbPun = False
          }
addFieldToRecord _ _ = Nothing

addFieldToRecordDataDefn ::
  String -> (String, BangType GhcPs) -> LHsDecl GhcPs -> Maybe (LHsDecl GhcPs)
addFieldToRecordDataDefn targetName (fieldName, fieldTy) decl = do
  (_ext, lName, _tyVars, _fixity, dataDefn) <- decl ^? l_val . _TyClD . _2 . _DataDecl
  guard $ lName ^. l_val == tyConName targetName

  constructors <- dataDefn ^? dd_cons . _DataTypeCons_data
  constructor <- case constructors of [c] -> pure c; _ -> Nothing

  fields <- constructor ^? l_val . _ConDeclH98 . con_args . _RecCon . l_val

  let
    fieldRootDelta =
      fromMaybe
        (deltaPos 0 1)
        (fields ^? _last . l_loc . ann . _EpAnn . epAnn_entry . anchor_op . _MovedAnchor)
    fieldTypeDelta =
      fromMaybe
        (deltaPos 0 1)
        ( fields
            ^? _last . l_val . cd_fld_type . l_loc . ann . _EpAnn . epAnn_entry . anchor_op . _MovedAnchor
        )

  let
    fields' =
      addTrailingComma (deltaPos 0 0) fields
        <> [ L
              ( SrcSpanAnn
                  (EpAnn generatedAnchor{GHC.anchor_op = MovedAnchor fieldRootDelta} (AnnListItem []) emptyComments)
                  generatedSrcSpan
              )
              $ ConDeclField
                (EpAnn generatedAnchor [AddEpAnn AnnDcolon (EpaDelta (deltaPos 0 1) [])] emptyComments)
                [L noSrcSpanA . FieldOcc NoExtField . L noSrcSpanA . mkRdrUnqual $ mkVarOcc fieldName]
                ( L
                    ( SrcSpanAnn
                        (EpAnn generatedAnchor{GHC.anchor_op = MovedAnchor fieldTypeDelta} (AnnListItem []) emptyComments)
                        generatedSrcSpan
                    )
                    fieldTy
                )
                Nothing
           ]

    dataDefn' =
      dataDefn
        & dd_cons . _DataTypeCons_data
          .~ [ constructor
                & l_val . _ConDeclH98 . con_args . _RecCon . l_val
                  .~ fields'
             ]

  pure $
    decl
      & l_val . _TyClD . _2 . _DataDecl . _5 .~ dataDefn'

addTrailingComma ::
  DeltaPos ->
  [GenLocated SrcSpanAnnA e] ->
  [GenLocated SrcSpanAnnA e]
addTrailingComma _ [] = []
addTrailingComma commaDelta [L (SrcSpanAnn epa ss) field] =
  [ L
      ( SrcSpanAnn
          ( addTrailingAnnToA
              ss
              (AddCommaAnn $ EpaDelta commaDelta [])
              emptyComments
              epa
          )
          ss
      )
      field
  ]
addTrailingComma commaDelta (x : xs) =
  let commaDelta' =
        case GHC.ann $ getLoc x of
          EpAnnNotUsed -> commaDelta
          EpAnn _anc (AnnListItem trailings) _comments ->
            case mapMaybe (\case AddCommaAnn loc -> Just loc; _ -> Nothing) trailings of
              [] -> commaDelta
              loc : _ ->
                case loc of
                  EpaSpan{} -> commaDelta
                  EpaDelta delta _comments -> delta
  in x : addTrailingComma commaDelta' xs

openCDelta :: EpAnn [AddEpAnn] -> Maybe DeltaPos
openCDelta a = do
  anns <- a ^? _EpAnn . epAnn_anns
  let anns' = mapMaybe (\case AddEpAnn AnnOpenC loc -> Just loc; _ -> Nothing) anns
  anns' ^? _head . _EpaDelta . _1

recFieldDelta :: LHsRecField GhcPs (LHsExpr GhcPs) -> DeltaPos
recFieldDelta (L loc (HsFieldBind _ext _lhs _rhs _pun)) =
  case GHC.anchor_op . GHC.entry . GHC.ann $ loc of
    UnchangedAnchor -> error "recFieldDelta: UnchangedAnchor"
    MovedAnchor delta -> delta

recFieldRhsDelta :: LHsRecField GhcPs (LHsExpr GhcPs) -> DeltaPos
recFieldRhsDelta (L _ (HsFieldBind _ext _lhs rhs _pun)) =
  case GHC.anchor_op . GHC.entry . GHC.ann $ getLoc rhs of
    UnchangedAnchor -> error "recFieldRhsDelta: UnchangedAnchor"
    MovedAnchor delta -> delta
