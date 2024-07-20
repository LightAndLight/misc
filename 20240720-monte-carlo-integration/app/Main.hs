{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import System.Random.Stateful (StatefulGen, Uniform, UniformRange)
import qualified System.Random.Stateful
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans (MonadTrans)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (replicateM)
import Data.Foldable (for_, foldl')
import Data.Maybe (mapMaybe)

approximatePi :: MonadRandom m => Int -> m Double
approximatePi numSamples = do
  coords <- replicateM numSamples ((,) <$> uniformR (xmin, xmax) <*> uniformR (ymin, ymax))
  let included = filter isIncluded coords
  let ratio = fromIntegral (length included) / fromIntegral (length coords)
  pure $ ratio * totalArea
  where
    xmin, xmax :: Double
    xmin = -1
    xmax = 1

    ymin, ymax :: Double
    ymin = -1
    ymax = 1
    
    totalArea :: Double
    totalArea = (xmax - xmin) * (ymax - ymin)

    isIncluded :: (Double, Double) -> Bool
    isIncluded (x, y) = x**2 + y**2 <= 1

data Linear a = Linear { gradient :: a, intercept :: a }

applyLinear :: Num a => Linear a -> a -> a
applyLinear (Linear m c) x = m * x + c

integrateLinear :: (MonadRandom m, MonadIO m) => Int -> (Double, Double) -> Linear Double -> m Double
integrateLinear numSamples (xmin, xmax) f = do
  coords <- replicateM numSamples ((,) <$> uniformR (xmin, xmax) <*> uniformR (ymin, ymax))
  let included = mapMaybe isIncluded coords
  let ratio = sum included / fromIntegral (length coords)
  pure $ ratio * totalArea
  where
    ymin, ymax :: Double
    (ymin, ymax) = 
      let
        v1 = min 0 $ applyLinear f xmin
        v2 = max 0 $ applyLinear f xmax
      in
        if gradient f < 0 then (v2, v1) else (v1, v2)
    
    totalArea :: Double
    totalArea = (xmax - xmin) * (ymax - ymin)

    isIncluded :: (Double, Double) -> Maybe Double
    isIncluded (x, y) = 
      let fx = applyLinear f x in
      if fx < 0
      then if fx <= y && y <= 0 then Just (-1) else Nothing
      else if 0 <= y && y <= fx then Just 1 else Nothing

main :: IO ()
main = do
  putStrLn $ "pi: " <> show (pi :: Double)
  for_ [0::Int ..5] $ \n -> do
    let samples = 10^n
    result <- approximatePi samples
    putStrLn $ show samples <> " samples: " <> show result

  putStrLn "\ny = x from -1 to 1 (expected answer: 0)"
  do
    let f = Linear 1 0
    let range = (-1, 1)
    for_ [0::Int ..5] $ \n -> do
      let samples = 10^n
      result <- integrateLinear samples range f
      putStrLn $ show samples <> " samples: " <> show result
  
  putStrLn "\ny = 2x + 1 from 0.5 to 2 (expected answer: (2^2 + 2) - (0.5^2 + 0.5) = 6 - 0.75 = 5.25)"
  do
    let f = Linear 2 1
    let range = (0.5, 2)
    for_ [0::Int ..5] $ \n -> do
      let samples = 10^n
      result <- integrateLinear samples range f
      putStrLn $ show samples <> " samples: " <> show result

-- Better random generator interface

class Monad m => MonadRandom m where
  uniform :: Uniform a => m a
  uniformR :: UniformRange a => (a, a) -> m a

instance MonadRandom IO where
  uniform = System.Random.Stateful.uniformM System.Random.Stateful.globalStdGen
  uniformR x = System.Random.Stateful.uniformRM x System.Random.Stateful.globalStdGen

newtype RandomT g m a = RandomT { unRandomT :: ReaderT g m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

runRandomT :: g -> RandomT g m a -> m a
runRandomT g ma = runReaderT (unRandomT ma) g

instance StatefulGen g m => MonadRandom (RandomT g m) where
  uniform = RandomT $ ReaderT System.Random.Stateful.uniformM
  uniformR x = RandomT . ReaderT $ System.Random.Stateful.uniformRM x

-- Making sense of stats concepts

newtype UnitInterval = UnitInterval Double

mkUnitInterval :: Double -> UnitInterval
mkUnitInterval x
  | 0.0 <= x && x <= 1.0 = UnitInterval x
  | otherwise = error $ show x <> " outside [0, 1]"

unitIntervalValue :: UnitInterval -> Double
unitIntervalValue (UnitInterval x) = x

zero :: UnitInterval
zero = UnitInterval 0.0

one :: UnitInterval
one = UnitInterval 1.0

data RandomVariable a
  = RandomVariable
  { f :: a -> Double
  , fImage :: Double -> a
  , cdf :: Double -> UnitInterval
  , inverseCdf :: UnitInterval -> Double
  }

mapRV :: (a -> b) -> (b -> a) -> RandomVariable a -> RandomVariable b
mapRV to from (RandomVariable f fImage cdf inverseCdf) = RandomVariable (f . from) (to . fImage) cdf inverseCdf

sample :: MonadRandom m => RandomVariable a -> m a
sample rv = do
  x <- uniformR (0.0 :: Double, 1.0)
  pure $ fImage rv (inverseCdf rv $ UnitInterval x)

always :: a -> RandomVariable a
always a =
  RandomVariable
  { f = const infinity
  , fImage = \x -> if x < k then undefined else a
  , cdf = \x -> if x < k then zero else one
  , inverseCdf = \(unitIntervalValue -> _) -> k
  }
  where
    infinity = 1 / 0
    k = 0.0

bool :: RandomVariable Bool
bool =
  RandomVariable
  { f = \x -> if x then 1.0 else 0.0
  , fImage = \x -> if x < 0.0 then undefined else x >= 1.0
  , cdf = \x -> if x < 0.0 then zero else if x < 1.0 then mkUnitInterval 0.5 else one
  , inverseCdf = \(unitIntervalValue -> x) -> if x < 0.5 then 0.0 else 1.0
  }

expectedValue :: Fractional a => MonadRandom m => Int -> RandomVariable a -> m a
expectedValue numSamples rv = do
  samples <- replicateM numSamples (sample rv)
  pure . (/ fromIntegral numSamples) . sum $ samples
