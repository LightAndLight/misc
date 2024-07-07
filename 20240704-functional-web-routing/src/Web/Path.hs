{-# language DataKinds #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
module Web.Path (
  Path,
  IsPart(..),
  type (//),
  (//),
  end,
  segment,
  param,

  -- * Internals
  Part(..),
  eqPart,
  Path(..)
) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(..), (:~~:)(..))
import Data.String (IsString(..))
import Text.Read (readMaybe)
import Type.Reflection (Typeable, TypeRep, eqTypeRep, typeRep)
import Web.Param

class Typeable a => IsPart a where
  encodePart :: a -> String
  decodePart :: String -> Maybe a

instance IsPart Int where
  encodePart = show
  decodePart = readMaybe

instance IsPart () where
  encodePart () = ""
  
  decodePart "" = Just ()
  decodePart _ = Nothing

eqPart :: Part a -> Part b -> Maybe (a :~: b)
eqPart (PartString s) (PartString s') = if s == s' then Just Refl else Nothing
eqPart (PartVar _ ty) (PartVar _ ty') = do
  HRefl <- eqTypeRep ty ty'
  pure Refl
eqPart _ _ = Nothing

type family (//) (a :: Type) (as :: [Type]) :: [Type] where
  (//) () a = a
  (//) (Param a) b = Param a ': b

data Path (as :: [Type]) where
  PathNil :: Path '[]
  PathCons :: Part a -> Path as -> Path (a // as)

data Part a where
  PartString :: String -> Part ()
  PartVar:: IsPart a => String -> TypeRep a -> Part (Param a)

instance a ~ () => IsString (Part a) where
  fromString = PartString

(//) :: Part a -> Path as -> Path (a // as)
(//) = PathCons

infixr 5 //

end :: Path '[]
end = PathNil

param :: String -> forall a. IsPart a => Part (Param a)
param name = PartVar name typeRep

segment :: String -> Part ()
segment = PartString
