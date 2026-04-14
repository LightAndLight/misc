{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Codec.Picture
  ( Image
  , PixelRGB8 (..)
  , PixelRGBF (..)
  , convertRGB8
  , generateImage
  , imageHeight
  , imageWidth
  , pixelAt
  , readJpeg
  , writePng
  )
import Codec.Picture.Types (pixelFold, pixelMapXY)
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.Async (replicateConcurrently_, replicateConcurrently)
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Concurrent.STM.TVar (modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Data.Function (on)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (minimumBy)
import Data.Monoid (Ap (..))
import qualified Data.Vector.Storable as Storable
import Debug.Trace (trace, traceShow)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable (..))
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)
import Data.Foldable (fold)
import Data.Int (Int32)

instance Storable PixelRGBF where
  sizeOf _ = 3 * sizeOf (undefined :: Float)
  alignment _ = alignment (undefined :: Float)
  peek ptr =
    PixelRGBF
      <$> peekElemOff (castPtr ptr) 0
      <*> peekElemOff (castPtr ptr) 1
      <*> peekElemOff (castPtr ptr) 2
  poke ptr (PixelRGBF r g b) = do
    pokeElemOff (castPtr ptr) 0 r
    pokeElemOff (castPtr ptr) 1 g
    pokeElemOff (castPtr ptr) 2 b

imagePixels :: Image PixelRGB8 -> Storable.Vector Pixel
imagePixels image =
  Storable.generate
    (width * imageHeight image)
    ( \ix ->
        let
          x = ix `mod` width
          y = ix `div` width
        in
          Pixel (rgb8ToFloat $ pixelAt image x y) (fromIntegral x) (fromIntegral y)
    )
  where
    width = imageWidth image

limit = 20

main :: IO ()
main = do
  (read -> k) : (read -> colorWeight) : path : _ <- getArgs
  image <- either error pure =<< readJpeg path
  let rgbImage = convertRGB8 image

  means <- Storable.replicateM k $ do
    x <- randomRIO (0, imageWidth rgbImage)
    y <- randomRIO (0, imageHeight rgbImage)
    let p = pixelAt rgbImage x y
    pure (Pixel (rgb8ToFloat p) (fromIntegral x) (fromIntegral y))

  putStrLn $ "initial means: " ++ show means
  let means' = veryNaiveKmeans' colorWeight limit (imagePixels rgbImage) means
  putStrLn $ "final means: " ++ show means'
  let
    histogram =
      pixelFold
        ( \acc x y pixel ->
            let
              cluster =
                fst
                  . minimumBy (compare `on` snd)
                  . fmap
                    ( \(ix, centroid) ->
                        let
                          !dist =
                            pixelDistanceSq colorWeight centroid (Pixel (rgb8ToFloat pixel) (fromIntegral x) (fromIntegral y))
                        in
                          (ix, dist)
                    )
                  $ zip [0 ..] (Storable.toList means')
            in
              IntMap.insertWith (+) cluster (1 :: Int) acc
        )
        IntMap.empty
        rgbImage

  putStrLn $ "histogram: " ++ show histogram

  let
    rgbImage' =
      pixelMapXY
        ( \x y pixel ->
            let
              Pixel (PixelRGBF r' g' b') _x _y =
                fst
                  . minimumBy (compare `on` snd)
                  . fmap
                    ( \centroid ->
                        let !dist =
                              pixelDistanceSq colorWeight centroid (Pixel (rgb8ToFloat pixel) (fromIntegral x) (fromIntegral y))
                        in (centroid, dist)
                    )
                  $ Storable.toList means'
            in
              PixelRGB8 (floor r') (floor g') (floor b')
        )
        rgbImage

  do
    let outPath = path <> "-k" <> show k <> "-cw" <> show colorWeight <> ".png"
    writePng outPath rgbImage'
    putStrLn $ "wrote " ++ outPath

add :: PixelRGBF -> PixelRGBF -> PixelRGBF
add (PixelRGBF r g b) (PixelRGBF r' g' b') = PixelRGBF (r + r') (g + g') (b + b')

scale :: Float -> PixelRGBF -> PixelRGBF
scale x (PixelRGBF r g b) = PixelRGBF (x * r) (x * g) (x * b)

data Mean
  = Mean
      -- | Value
      !Pixel
      -- | Count
      !Int32
  deriving (Show, Eq)

instance Storable Mean where
  sizeOf _ = sizeOf (undefined :: Pixel) + sizeOf (undefined :: Int32)
  alignment _ = alignment (undefined :: Int32)
  peek ptr =
    Mean <$>
      peek (castPtr ptr) <*>
      peek (ptr `plusPtr` sizeOf (undefined :: Pixel))
  poke ptr (Mean pix count) = do
    poke (castPtr ptr) pix
    poke (ptr `plusPtr` sizeOf (undefined :: Pixel)) count

getMean :: Mean -> Pixel
getMean (Mean value count) =
  Pixel
    (scale invCnt (color value))
    (invCnt * posX value)
    (invCnt * posY value)
  where
    invCnt = 1 / fromIntegral count

instance Semigroup Mean where
  Mean (Pixel pix x y) b <> Mean (Pixel pix' x' y') b' = Mean (Pixel (pix `add` pix') (x + x') (y + y')) (b + b')

instance Monoid Mean where
  mempty = Mean (Pixel (PixelRGBF 0 0 0) 0 0) 0

data Pixel = Pixel {color :: !PixelRGBF, posX :: !Float, posY :: !Float}
  deriving (Show, Eq)

instance Storable Pixel where
  sizeOf _ = sizeOf (undefined :: PixelRGBF) + 2 * sizeOf (undefined :: Float)
  alignment _ = alignment (undefined :: Float)
  peek ptr =
    Pixel
      <$> peek (castPtr ptr)
      <*> peek (ptr `plusPtr` sizeOf (undefined :: PixelRGBF))
      <*> peek (ptr `plusPtr` sizeOf (undefined :: PixelRGBF) `plusPtr` sizeOf (undefined :: Float))
  poke ptr (Pixel val x y) = do
    poke (castPtr ptr) val
    poke (ptr `plusPtr` sizeOf (undefined :: PixelRGBF)) x
    poke (ptr `plusPtr` sizeOf (undefined :: PixelRGBF) `plusPtr` sizeOf (undefined :: Float)) y

distanceSq :: PixelRGBF -> PixelRGBF -> Float
distanceSq (PixelRGBF r g b) (PixelRGBF r' g' b') =
  (r - r') ^ (2 :: Int) + (g - g') ^ (2 :: Int) + (b - b') ^ (2 :: Int)

pixelDistanceSq :: Float -> Pixel -> Pixel -> Float
pixelDistanceSq colorWeight (Pixel pixel x y) (Pixel pixel' x' y') =
  colorWeight * (pixel `distanceSq` pixel')
    + (1 - colorWeight) * ((x - x') ^ (2 :: Int) + (y - y') ^ (2 :: Int))

rgb8ToFloat :: PixelRGB8 -> PixelRGBF
rgb8ToFloat (PixelRGB8 r g b) = PixelRGBF (fromIntegral r) (fromIntegral g) (fromIntegral b)

parFoldMap :: (Storable a, Monoid m) => (a -> m) -> Storable.Vector a -> m
parFoldMap f fa = unsafePerformIO go
  where
    {-# NOINLINE go #-}
    go = do
      queue <- newTQueueIO
      doneVar <- newTVarIO False

      threads <- getNumCapabilities
      _threadId <- forkIO $ do
        getAp $ Storable.foldMap (Ap . atomically . writeTQueue queue) fa
        atomically $ writeTVar doneVar True
      result <- replicateConcurrently threads $ do
        resultVar <- newTVarIO mempty
        let
          loop = do
            ma <- atomically $ do
              Just <$> readTQueue queue <|> do
                done <- readTVar doneVar
                if done then pure Nothing else retry
            case ma of
              Nothing -> pure ()
              Just a -> do
                let !m = f a
                atomically $ modifyTVar' resultVar (<> m)
                loop

        loop
        readTVarIO resultVar

      pure $ fold result

newtype MonoidalIntMap a = MonoidalIntMap {getMonoidalIntMap :: IntMap a}

instance Semigroup a => Semigroup (MonoidalIntMap a) where
  MonoidalIntMap a <> MonoidalIntMap b = MonoidalIntMap (IntMap.unionWith (<>) a b)

instance Semigroup a => Monoid (MonoidalIntMap a) where
  mempty = MonoidalIntMap IntMap.empty

veryNaiveKmeans ::
  -- | Color distance weight
  Float ->
  -- | Iteration limit
  Int ->
  -- | Image data
  Storable.Vector Pixel ->
  {-| Initial means

    @length means == k@
  -}
  Storable.Vector Pixel ->
  {-| Final means

    @length means == k@
  -}
  Storable.Vector Pixel
veryNaiveKmeans colorWeight limit image = go 0
  where
    go :: Int -> Storable.Vector Pixel -> Storable.Vector Pixel
    go iteration start
      | iteration > limit = start
      | otherwise =
          let
            end = step $! trace ("iteration " ++ show iteration) start
          in
            if start == end
              then end
              else go (iteration + 1) end

    step start =
      let
        clusters =
          getMonoidalIntMap $
            Storable.foldMap
              ( \pixel ->
                  let
                    index =
                      fst
                        . minimumBy (compare `on` snd)
                        . fmap
                          ( \(ix, centroid) ->
                              let
                                !dist = pixelDistanceSq colorWeight centroid pixel
                              in
                                (ix, dist)
                          )
                        $ zip [0 ..] (Storable.toList start)
                  in
                    MonoidalIntMap $ IntMap.singleton index (Mean pixel 1)
              )
              image
      in
        Storable.imap (\ix centroid -> maybe centroid getMean $ IntMap.lookup ix clusters) start

data PixelWithInt
  = PixelWithInt !Pixel !Int32
  deriving Show

instance Storable PixelWithInt where
  sizeOf _ = sizeOf (undefined :: Pixel) + sizeOf (undefined :: Int32)
  alignment _ = alignment (undefined :: Int32)
  peek ptr =
    PixelWithInt  <$>
      peek (castPtr ptr) <*>
      peek (ptr `plusPtr` sizeOf (undefined :: Pixel))
  poke ptr (PixelWithInt  pix count) = do
    poke (castPtr ptr) pix
    poke (ptr `plusPtr` sizeOf (undefined :: Pixel)) count

veryNaiveKmeans' ::
  -- | Color distance weight
  Float ->
  -- | Iteration limit
  Int ->
  -- | Image data
  Storable.Vector Pixel ->
  {-| Initial means

    @length means == k@
  -}
  Storable.Vector Pixel ->
  {-| Final means

    @length means == k@
  -}
  Storable.Vector Pixel
veryNaiveKmeans' colorWeight limit image = go 0
  where
    go :: Int -> Storable.Vector Pixel -> Storable.Vector Pixel
    go iteration start
      | iteration > limit = start
      | otherwise =
          let
            end = step $! trace ("iteration " ++ show iteration) start
          in
            if start == end
              then end
              else go (iteration + 1) end

    step start =
      let
        clusteredPixels :: Storable.Vector Int32
        clusteredPixels =
          Storable.map
            ( \pixel ->
                  fst
                    . minimumBy (compare `on` snd)
                    . fmap
                      ( \(ix, centroid) ->
                          let
                            !dist = pixelDistanceSq colorWeight centroid pixel
                          in
                            (ix, dist)
                      )
                    $ zip [0::Int32 ..] (Storable.toList start)
            )
            image

        end :: Storable.Vector Pixel
        end = Storable.generate (Storable.length start) $ \ix ->
          let
            result =
              Storable.ifoldl'
                (\acc pos pix ->
                  let ix' = clusteredPixels Storable.! pos in
                  if fromIntegral ix == ix' then acc <> Mean pix 1 else acc
                )
                mempty
                image
          in
            if result == mempty
            then start Storable.! ix
            else getMean result
      in
        end
