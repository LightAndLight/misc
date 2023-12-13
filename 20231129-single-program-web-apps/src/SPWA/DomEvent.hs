{-# LANGUAGE OverloadedStrings #-}

module SPWA.DomEvent (DomEvent (..), renderDomEvent) where

import Data.String (IsString)

data DomEvent
  = Click
  | Change
  deriving (Eq, Ord)

renderDomEvent :: (IsString s) => DomEvent -> s
renderDomEvent Click = "\"click\""
renderDomEvent Change = "\"change\""