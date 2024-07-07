{-# language FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Web.Link where

import Data.Kind (Type)
import Web.Body (Body)
import Web.Param (Param)
import Web.Response (Response)
import Data.List (intercalate)

type family LinkFor (a :: Type) (x :: Type):: Type where
  LinkFor (Body a -> b) x = x
  LinkFor (Param a -> b) x = a -> LinkFor b x
  LinkFor (Response a) x = x

newtype Link a = Link { getLink :: forall r. ([String] -> r) -> LinkFor a r }

class MkLink a where
  mkLink :: LinkFor a [String] -> Link a

instance MkLink b => MkLink (Param a -> b) where
  mkLink f = Link (\g a -> getLink (mkLink @b (f a)) g)

instance MkLink (Body a -> b) where
  mkLink x = Link (\f -> f x)

instance MkLink (Response a) where
  mkLink x = Link (\f -> f x)

url :: Link a -> LinkFor a String
url l = getLink l (intercalate "/")
