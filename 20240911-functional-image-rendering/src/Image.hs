{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
module Image where

import Data.Bits (xor)
import Data.Kind (Type)
import Data.Word (Word32, Word8)
import qualified Codec.Picture as JuicyPixels
import qualified Data.Vector.Storable as Storable

data Point2d = Point2d {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Show)

data Vec2d = Vec2d {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Show)

data RGB
  = RGB
  {-# UNPACK #-} !Double
  {-# UNPACK #-} !Double
  {-# UNPACK #-} !Double

data Rect
  = Rect
  -- | Left
  {-# UNPACK #-} !Double
  -- | Top
  {-# UNPACK #-} !Double
  -- | Width
  {-# UNPACK #-} !Double
  -- | Height
  {-# UNPACK #-} !Double

translateRect :: Vec2d -> Rect -> Rect
translateRect (Vec2d dx dy) (Rect l t w h) = Rect (l + dx) (t + dy) w h

sampleRect :: Image.Rect -> Point2d -> Bool
sampleRect (Image.Rect l t w h) (Point2d x y) =
  l <= x && x <= l + w && t >= y && y >= t - h

intersectRect :: Rect -> Rect -> Maybe Rect
intersectRect (Rect l t w h) (Rect l' t' w' h') =
  if intersects
  then Just $ Rect (max l l') (min t t') (min r r' - max l l') (min t t' - max b b')
  else Nothing
  where
    r = l + w
    r' = l' + w'
    b = t - h
    b' = t' - h'
    intersects =
      l <= r' && r' >= l && t <= b' && b' >= t

class Image i where
  type Colour i :: Type
  image :: (Point2d -> Colour i) -> i
  sample :: i -> Point2d -> Colour i

  constant :: Colour i -> i
  map :: (Colour i -> Colour i) -> i -> i
  map2 :: (Colour i -> Colour i -> Colour i) -> i -> i -> i
  
  translate :: Vec2d -> i -> i

rasterise :: Image i => i -> Rect -> Word32 -> Word32 -> [Colour i]
rasterise input (Rect rX rY rWidth rHeight) width height = do
  let dX = rWidth / fromIntegral width
  let dY = rHeight / fromIntegral height
  y <- [0..height-1]
  x <- [0..width-1]
  pure $ sample input (Point2d (rX + fromIntegral x * dX) (rY - fromIntegral y * dY))

rasteriseJuicy ::
  (Image i, JuicyPixels.Pixel a, Colour i ~ JuicyPixels.PixelBaseComponent a) =>
  i ->
  Rect ->
  Word32 ->
  Word32 ->
  JuicyPixels.Image a
rasteriseJuicy input region width height =
  JuicyPixels.Image
    (fromIntegral width)
    (fromIntegral height)
    (Storable.fromList $ rasterise input region width height)

rasteriseJuicyY8 ::
  (Image i, Colour i ~ Word8) =>
  i ->
  Rect ->
  Word32 ->
  Word32 ->
  JuicyPixels.Image JuicyPixels.Pixel8
rasteriseJuicyY8 = rasteriseJuicy

vstripes :: (Image i, Colour i ~ Bool) => i
vstripes =
  image (\(Point2d x _) -> even (floor (x / 2) :: Int))

hstripes :: (Image i, Colour i ~ Bool) => i
hstripes =
  image (\(Point2d _ y) -> even (floor (y / 2) :: Int))

checkerboard :: (Image i, Colour i ~ Bool) => i
checkerboard =
  map2 xor vstripes hstripes

circle :: (Image i, Colour i ~ Bool) => Double -> i
circle r =
  image (\(Point2d x y) -> x**2 + y**2 <= r)

rectangle ::
  (Image i, Colour i ~ Bool) =>
  Double ->
  Double ->
  Double ->
  Double ->
  i
rectangle l t w h =
  image (sampleRect $ Rect l t w h)
