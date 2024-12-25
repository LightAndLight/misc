module Compiler.Kind where

data Kind
  = -- | @Type@
    KType
  | -- | @k1 -> k2@
    KArrow Kind Kind
  | -- | @_@
    KUnknown
  deriving (Show, Eq)
