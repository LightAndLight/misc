{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
module Image.X11 where

import Image (Image(..), Point2d (..), translateRect, sampleRect, Rect (..), intersectRect)
import Image.Pure (PureImage (..))
import Prelude hiding (map)
import Graphics.X11 (Display, Drawable, GC, drawPoints, coordModeOrigin, setBackground, setForeground, drawRectangle)
import Data.Word (Word32)
import Data.Int (Int32)
import Control.Monad (guard)
import qualified Graphics.X11 as X11
import Data.Foldable (for_)
import Data.Bool (bool)

data X11Image
  = DrawImage (PureImage Bool)
  | DrawRect
      -- | Fill color
      Bool
      -- | Top-left
      !Rect

data X11Transform = X11Transform

instance Image X11Image where
  type Colour X11Image = Bool
  
  image = DrawImage . image
  
  sample (DrawImage f) = sample f
  sample (DrawRect fill rect) = bool (not fill) fill . sampleRect rect

  constant = DrawImage . constant
  
  map f (DrawImage i) = DrawImage (map f i)
  map f (DrawRect fill rect) = DrawRect (f fill) rect
  
  map2 f a b =
    DrawImage $ map2 f (image $ sample a) (image $ sample b)

  translate v (DrawImage i) =
    DrawImage (translate v i)
  translate v (DrawRect fill rect) =
    DrawRect fill (translateRect v rect)

render ::
  X11Image ->
  Image.Rect ->
  Word32 ->
  Word32 ->
  Display ->
  Drawable ->
  GC ->
  (Bool -> X11.Pixel) ->
  IO ()
render (DrawImage i) (Rect rx ry rwidth rheight) width height display drawable gc toPixel = do
  setBackground display gc (toPixel False)
  setForeground display gc (toPixel True)
  
  let dx = rwidth / fromIntegral width
  let dy = rheight / fromIntegral height
  let
    points = do
      y <- [0..height-1]
      x <- [0..width-1]
      guard $ sample i (Point2d (rx + fromIntegral x * dx) (ry - fromIntegral y * dy))
      pure $ X11.Point (fromIntegral @Word32 @Int32 x) (fromIntegral @Word32 @Int32 y)
  drawPoints display drawable gc points coordModeOrigin
render (DrawRect fill rect) extent@(Rect _ _ ewidth eheight) width height display drawable gc toPixel = do
  setBackground display gc (toPixel fill)
  setForeground display gc (toPixel $ not fill)

  let dx = ewidth / fromIntegral width
  let dy = eheight / fromIntegral height
  for_ (intersectRect rect extent) $ \(Image.Rect rx ry rwidth rheight) -> do
    drawRectangle
      display
      drawable
      gc
      (floor (rx / dx))
      (floor (ry / dy))
      (floor (rwidth / dx))
      (floor (rheight / dy))
