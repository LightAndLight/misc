{-# LANGUAGE BangPatterns #-}

module SPWA.Event (Event, sample, domEvent) where

import SPWA.Behavior (Behavior)
import SPWA.DomEvent (DomEvent (..))
import SPWA.Element (Element (..))
import SPWA.Js (Js (..))
import SPWA.MemoRef (unsafeNewMemoRef)
import SPWA.PageBuilder (Event (..), PageBuilder, initBehavior, memoEventWith, notify, subscribe)
import SPWA.Supply (freshId)

{- |
For example, in this code

@
do
  rec r <- stepper initial $ sample e (current r)
  let r' = fmap f r
  ...
@

there's only one call to @sample e (current r)@. But without memoization,
two @sample e (current r)@ events will be compiled: one for the event that
updates to @current r@, and one for the event behind @fmap f r@. As a result,
@e@ will have two subscribers instead of one. This is okay semantically, but
it increases code size and duplicates work.

Functions that call 'memoEvent' should be marked as @NOINLINE@, so that each
call-site is assigned a single memoized event.
-}
memoEvent :: (String -> PageBuilder ()) -> Event a
memoEvent f = let !memoRef = unsafeNewMemoRef f in Event $ memoEventWith memoRef f

{-# NOINLINE sample #-}
sample :: Event a -> Behavior b -> Event (a, b)
sample ea bb =
  memoEvent $ \name -> do
    b <- initBehavior bb
    temp <- ("temp_" <>) <$> freshId
    notifyJs <- notify name
    subscribe ea $ \value ->
      Js ["const " <> temp <> " = { fst: " <> value <> ", snd: " <> b <> " };"]
        <> notifyJs temp

domEvent :: DomEvent -> Element -> Event ()
domEvent de (MkElement elId _) = FromDomEvent elId de