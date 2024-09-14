{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Image.Pure where

import Image (Point2d (..), Image(..), Vec2d (..))
import Prelude hiding (map)

newtype PureImage a = PureImage (Point2d -> a)
  deriving (Functor, Applicative)

newtype PureTransform = PureTransform (Point2d -> Point2d)

instance Image (PureImage a) where
  type Colour (PureImage a) = a
  image = PureImage
  sample (PureImage f) = f

  constant = pure
  map = fmap
  map2 = liftA2
  
  translate (Vec2d dx dy) (PureImage i) =
    PureImage $ \(Point2d x y) -> i (Point2d (x - dx) (y - dy))
