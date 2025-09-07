-- https://docs.gtk.org/gtk4/getting_started.html#basics
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.GI.Base.Signals (on)
import Data.Int (Int32)
import GI.GObject.Objects.Object (objectUnref)
import GI.Gio.Flags (ApplicationFlags (..))
import GI.Gio.Objects.Application (applicationRun)
import GI.Gtk.Enums (Align (..), Orientation (..))
import GI.Gtk.Objects.Application (applicationNew)
import GI.Gtk.Objects.ApplicationWindow (applicationWindowNew)
import GI.Gtk.Objects.Box (boxAppend, boxNew)
import GI.Gtk.Objects.Button (buttonNewWithLabel)
import GI.Gtk.Objects.Widget (widgetSetHalign, widgetSetValign)
import GI.Gtk.Objects.Window
  ( windowDestroy
  , windowPresent
  , windowSetChild
  , windowSetDefaultSize
  , windowSetTitle
  )
import System.Environment (getArgs)

main :: IO Int32
main = do
  app <- applicationNew (Just "io.ielliott.demo") [ApplicationFlagsDefaultFlags]

  -- https://hackage-content.haskell.org/package/haskell-gi-base-0.26.9/docs/Data-GI-Base-Signals.html
  _id <- on app #activate $ do
    window <- applicationWindowNew app
    windowSetTitle window (Just "Window")
    windowSetDefaultSize window 200 200

    box <- boxNew OrientationVertical 0
    widgetSetHalign box AlignCenter
    widgetSetValign box AlignCenter

    windowSetChild window (Just box)

    button <- buttonNewWithLabel "Hello World"
    _id <- on button #clicked $ do
      putStrLn "Hello, world!"
    _id <- on button #clicked $ do
      windowDestroy window

    boxAppend box button

    windowPresent window

  args <- getArgs
  status <- applicationRun app (Just args)
  objectUnref app

  pure status
