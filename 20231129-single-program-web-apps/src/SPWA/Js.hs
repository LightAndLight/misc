{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SPWA.Js (Js (..), line, indent) where

import Data.String (IsString (..))

newtype Js = Js {getJs :: [String]}
  deriving (Show, Semigroup, Monoid)

instance IsString Js where
  fromString = Js . lines

line :: String -> Js
line = Js . pure

indent :: Int -> Js -> Js
indent n (Js ls) = Js (fmap (replicate n ' ' <>) ls)