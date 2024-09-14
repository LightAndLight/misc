{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Graphics.X11.Xlib.Display (openDisplay, closeDisplay, whitePixel, blackPixel, defaultScreen, rootWindow)
import Control.Concurrent (threadDelay, forkIO, killThread)
import Graphics.X11.Xlib.Window (createSimpleWindow, destroyWindow, mapWindow, clearWindow)
import Graphics.X11.Xlib.Event (nextEvent, allocaXEvent, flush, selectInput)
import Graphics.X11.Xlib.Misc (setTextProperty)
import Graphics.X11.Xlib.Atom (wM_NAME)
import Graphics.X11.Xlib.Context (createGC, freeGC)
import Control.Monad (forever, unless, when)
import Control.Concurrent.STM.TChan (newTChanIO, writeTChan, tryReadTChan)
import Control.Concurrent.STM (atomically)
import Graphics.X11.Types (exposureMask, structureNotifyMask, buttonPressMask)
import Data.Bits ((.|.), xor)
import Graphics.X11.Xlib.Extras (getEvent, Event (..))
import Data.Int (Int32)
import Data.Word (Word32)
import Foreign.C.Types (CInt(..))
import Image (circle, Rect(..), rectangle, map2, translate, Vec2d(..))
import Data.Bool (bool)
import Image.X11 (X11Image, render)
import Data.Time.Clock (getCurrentTime, nominalDiffTimeToSeconds, diffUTCTime)
import Data.Fixed (Pico)
import Animation (oscillate, oscillateFrom)
import FRP.Mono (runMonoFRP, atB, MonoFRP, mkEvent, traceE)
import FRP (never, switcherB, accumE, tagB, time)
import qualified Graphics.X11 as X11

data WindowBounds
  = WindowBounds
  { windowX :: {-# UNPACK #-} !Int32
  , windowY :: {-# UNPACK #-} !Int32
  , windowWidth :: {-# UNPACK #-} !Word32
  , windowHeight :: {-# UNPACK #-} !Word32
  } deriving (Eq, Show)

loop :: Monad m => (forall r. (b -> m r) -> (a -> m r) -> a -> m r) -> a -> m b
loop f a = do
  result <- f (pure . Left) (pure . Right) a
  case result of
    Left b -> pure b
    Right a' -> loop f a'

x11Main ::
  WindowBounds ->
  IO () ->
  (Double -> IO X11Image) ->
  Rect ->
  IO ()
x11Main requestedBounds onClick image extent = do
  display <- openDisplay ":0"
  let screen = defaultScreen display
  root <- rootWindow display screen

  let black = blackPixel display screen
  let white = whitePixel display screen

  window <-
    createSimpleWindow
      display
      root
      (windowX requestedBounds)
      (windowY requestedBounds)
      (windowWidth requestedBounds)
      (windowHeight requestedBounds)
      1
      black
      white

  setTextProperty display window "Functional image rendering demo" wM_NAME

  mapWindow display window
  flush display

  gc <- createGC display window

  selectInput display window (exposureMask .|. structureNotifyMask .|. buttonPressMask)

  allocaXEvent $ \eventPtr -> do
    chan <- newTChanIO

    threadId <- forkIO . forever $ do
      nextEvent display eventPtr
      event <- getEvent eventPtr
      atomically $ writeTChan chan event

    let
      draw WindowBounds{windowWidth = width, windowHeight = height} targetWindow t = do
        clearWindow display targetWindow

        image' <- image t
        render image' extent width height display targetWindow gc (bool white black)

        -- Seems to be necessary
        flush display

      eventLoop = loop $ \_break continue (start, windowBounds) -> do
        frameStart <- getCurrentTime

        let t = realToFrac @Pico @Double $ nominalDiffTimeToSeconds (frameStart `diffUTCTime` start)
        
        mEvent <- atomically $ tryReadTChan chan
        windowBounds' <- case mEvent of
          Just ExposeEvent{ev_window = eventWindow} -> do
            draw windowBounds eventWindow t
            pure windowBounds
          Just ConfigureEvent
            { ev_window = eventWindow
            , ev_x = CInt newX
            , ev_y = CInt newY
            , ev_width = CInt newWidth
            , ev_height = CInt newHeight
            } -> do
              let windowBounds' = WindowBounds newX newY (fromIntegral @Int32 @Word32 newWidth) (fromIntegral @Int32 @Word32 newHeight)
              unless (windowBounds == windowBounds') $
                draw windowBounds eventWindow t
              pure windowBounds'
          Just ButtonEvent{ev_button = button, ev_same_screen = True}
            | button == X11.button1 -> do
                onClick
                pure windowBounds
          _ -> do
            draw windowBounds window t
            pure windowBounds
        frameEnd <- getCurrentTime

        let remaining = 0.016 - nominalDiffTimeToSeconds (frameEnd `diffUTCTime` frameStart)
        when (remaining > 0) $
          threadDelay . floor $ 1_000_000 * remaining
        continue (start, windowBounds')

    start <- getCurrentTime
    _ <- eventLoop (start, requestedBounds)
    killThread threadId

  freeGC display gc
  destroyWindow display window
  closeDisplay display

data Toggle
  = Stopped
      -- | Total duration for which we've stopped
      Double
      -- | Time at which we stopped
      Double
  | Started
      -- | Duration for which we were stopped
      Double
  deriving Show

main :: IO ()
main = do
  let requestedBounds = WindowBounds 0 0 640 480
  runMonoFRP
    (do
      (triggerClick, eClick) <- mkEvent

      eStop <-
        accumE
          (Started 0)
          ((\(t, ()) mStop ->
            case mStop of
              Started duration ->
                Stopped duration t
              Stopped duration t' ->
                Started (duration + (t - t'))
           ) <$>
            tagB (time @MonoFRP) eClick
          )
          
      rec
        bX <-
          switcherB
            (oscillate 0.25)
            ((\(x, stop) ->
              case stop of
                Started duration ->
                  oscillateFrom (-duration) 0.25
                Stopped _duration _t ->
                  pure x
             ) <$> tagB bX eStop)

      let
        bImage =
          (\x ->
            map2
              xor
              (translate (Vec2d x 0) (rectangle (-0.5) 0.5 1 1))
              (circle 1)
          )
           <$> bX


      let onClick = triggerClick ()
      pure (never @MonoFRP, (onClick, bImage))
    )
    (\(onClick, bImage) -> do
      {-
      let image = \t -> map2 xor (translate (Vec2d (sin (2 * pi * 0.5 * t)) 0) $ rectangle (-0.5) 0.5 1 1) (circle 1)
      -}
      let extent = Rect (-2) 2 4 4
      x11Main requestedBounds onClick (atB bImage) extent
    )
