{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module SPWA.Path (Path (..), renderPath, href) where

import Data.List (intercalate)
import Data.String (IsString (..))

newtype Path = Path [String]
  deriving (Eq, Ord, Semigroup, Monoid, Show)

href :: Path -> (String, String)
href (Path segments) = ("href", intercalate "/" segments)

instance IsString Path where
  fromString = Path . pure

renderPath :: (IsString s, Monoid s) => Path -> s
renderPath (Path segments) = go segments
 where
  go [] = mempty
  go [b] = fromString b
  go (b : bs) =
    fromString b <> "/" <> go bs