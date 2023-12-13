{-# LANGUAGE GADTs #-}

module SPWA.Reactive (Reactive (..), current) where

import SPWA.PageBuilder (Behavior (..), Reactive (..))
import SPWA.Send (Send)

current :: (Send a) => Reactive a -> Behavior a
current = Current