{-# language GADTs #-}
module Web.UrlTemplate (pathToUrlTemplate) where

import Type.Reflection ((:~~:)(..), eqTypeRep, typeRep)
import Web.Path

partToUrlTemplate :: Part a -> String
partToUrlTemplate (PartString s) = s
partToUrlTemplate (PartVar name ty) =
  case eqTypeRep ty (typeRep @()) of
    Just HRefl -> ""
    Nothing -> "{" <> name <> "}"

pathToUrlTemplate :: Path as -> [String]
pathToUrlTemplate PathNil = []
pathToUrlTemplate (PathCons part parts) =
  let part' = partToUrlTemplate part in
  let parts' = pathToUrlTemplate parts in
  part' : parts'
