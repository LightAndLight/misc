{-# language FlexibleContexts #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
module Main where

import Control.Monad ((<=<), replicateM, unless)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Inference.MCMC (MCMCConfig(..), mcmc, defaultMCMCConfig)
import Control.Monad.Bayes.Integrator (expectation, normalize)
import Control.Monad.Bayes.Sampler.Strict (sampler)
import Control.Monad.Bayes.Weighted
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.Foldable (for_, foldl')
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Exts (fromList)
import System.Process (readProcess)

data Plot
  = Plot
  { plotData :: [String]
  , plotCommands :: [String]
  , plotXMin, plotXMax :: Maybe Double
  , plotXTic :: Maybe Double
  }

setXMin, setXMax :: Double -> Plot -> Plot
setXMin v p = p{plotXMin = Just v}
setXMax v p = p{plotXMax = Just v}

setXTic :: Double -> Plot -> Plot
setXTic v p = p{plotXTic = Just v}

histogram :: [(String, Double)] -> Plot
histogram values =
  Plot
  { plotData = [foldMap (\(x, y) -> x ++ " " ++ show y ++ "\n") values]
  , plotXMin = Just (-0.5)
  , plotXMax = Nothing
  , plotXTic = Nothing
  , plotCommands =
      [ "set style data histogram"
      , "set style fill solid"
      , "set yrange [0:" ++ show (1.1 * maxY) ++ "]"
      , "plot '-' using 2:xtic(1) notitle"
      ]
  }
  where
    maxY = maximum $ fmap snd values

boxes :: [(String, Double)] -> Plot
boxes values =
  Plot
  { plotData = [foldMap (\(x, y) -> x ++ " " ++ show y ++ "\n") values]
  , plotXMin = Just 0
  , plotXMax = Nothing
  , plotXTic = Just 0.1
  , plotCommands =
      [ "set style fill solid"
      , "set yrange [0:" ++ show (if maxY == 0 then 1 else 1.1 * maxY) ++ "]"
      , "plot '-' using 1:2 with boxes notitle"
      ]
  }
  where
    maxY = maximum $ fmap snd values

plot :: Plot -> IO ()
plot p = do
  _ <-
    readProcess
      "gnuplot"
      ["-e", intercalate "; " $ "set term qt" : plotRanges ++ plotTics ++ plotCommands p ++ ["pause mouse"]]
      (intercalate "e\n" $ plotData p)
  pure ()
  where
    plotRanges =
      case (plotXMin p, plotXMax p) of
        (Nothing, Nothing) -> []
        (Just xmin, Nothing) -> ["set xrange [" ++ show xmin ++ ":]"]
        (Nothing, Just xmax) -> ["set xrange [:" ++ show xmax ++ "]"]
        (Just xmin, Just xmax) -> ["set xrange [" ++ show xmin ++ ":" ++ show xmax ++ "]"]

    plotTics =
      case plotXTic p of
        Nothing -> []
        Just xtic -> ["set xtic " ++ show xtic]

measureHistogram :: Measure Double -> IO Plot
measureHistogram =
  fmap (boxes . (fmap . first) show) .
  sampler . fmap (histogramToList . Control.Monad.Bayes.Class.histogram 100) . replicateM 100000 . runWeightedT

newtype Urn = Urn{ getUrn :: Int }
  deriving (Show, Eq, Ord)

data Ball = Black | White
  deriving (Show, Eq, Ord)

{- ITIaLA by David MacKay

Example 2.6. There are eleven urns labelled by u ∈ {0, 1, 2, . . . , 10}, each con-
taining ten balls. Urn u contains u black balls and 10 − u white balls.
Fred selects an urn u at random and draws N times with replacement
from that urn, obtaining nB blacks and N − nB whites. Fred’s friend,
Bill, looks on. If after N = 10 draws nB = 3 blacks have been drawn,
what is the probability that the urn Fred is using is urn u, from Bill’s
point of view? (Bill doesn’t know the value of u).
-}
itiala_example_2_6 :: MonadMeasure m => Int -> m Urn
itiala_example_2_6 draws = do
  u <- Urn <$> uniformD [0..10]
  
  -- Urn contains `u` black balls and `10 - u` white balls
  let p_black = fromIntegral (getUrn u) / 10
  
  balls <-
    replicateM draws $
    fmap (bool White Black) (bernoulli p_black)

  condition $ length (filter (== Black) balls) == 3

  pure u

{- ITIaLA by David MacKay

Example 2.6 (continued). Assuming again that Bill has observed nB = 3 blacks
in N = 10 draws, let Fred draw another ball from the same urn. What
is the probability that the next drawn ball is a black
-}
itiala_example_2_6_cont :: MonadMeasure m => Int -> m Ball
itiala_example_2_6_cont draws = do
  u <- itiala_example_2_6 draws
  
  let p_black = fromIntegral (getUrn u) / 10
  fmap (bool White Black) (bernoulli p_black)

{- ITIaLA by David MacKay

Example 2.7. Bill tosses a bent coin N times, obtaining a sequence of heads
and tails. We assume that the coin has a probability fH of coming up
heads; we do not know fH . If nH heads have occurred in N tosses, what
is the probability distribution of fH ? (For example, N might be 10, and
nH might be 3; or, after a lot more tossing, we might have N = 300 and
nH = 29.) What is the probability that the N +1th outcome will be a
head, given nH heads in N tosses?

`itiala_example_2_8 t n` is the distribution `P(next toss is heads | saw n heads in t tosses)`,
when the prior distribution for `P(next toss is heads)` was uniform over [0, 1].

This is a PDF with domain [0, 1].

Expected value is Integral (x * f(x)) dx
-}
itiala_example_2_8 :: MonadMeasure m => Int -> Int -> m Double
itiala_example_2_8 tosses n = do
  fH <- random
  results <- replicateM tosses (bernoulli fH)
  condition $ length (filter id results) == n
  pure fH

itiala_example_2_8' :: MonadMeasure m => Int -> Int -> m Double
itiala_example_2_8' tosses n = beta (fromIntegral $ n + 1) (fromIntegral $ tosses - n + 1)

class VectorSpace a where
  type Scalar a :: Type
  scale :: Scalar a -> a -> a
  zero :: a
  add :: a -> a -> a

data V3 a = V3 !a !a !a
  deriving Show

instance Num a => Num (V3 a) where
  fromInteger = V3 <$> fromInteger <*> fromInteger <*> fromInteger
  (+) (V3 a b c) (V3 a' b' c') = V3 (a + a') (b + b') (c + c')
  (*) (V3 a b c) (V3 a' b' c') = V3 (a * a') (b * b') (c * c')

instance Num a => VectorSpace (V3 a) where
  type Scalar (V3 a) = a
  scale s (V3 a b c) = V3 (s*a) (s*b) (s*c)
  zero = V3 0 0 0
  add = (+)

data V2 a = V2 !a !a
  deriving Show

instance Num a => Num (V2 a) where
  fromInteger = V2 <$> fromInteger <*> fromInteger
  (+) (V2 a b) (V2 a' b') = V2 (a + a') (b + b')
  (*) (V2 a b) (V2 a' b') = V2 (a * a') (b * b')

instance Num a => VectorSpace (V2 a) where
  type Scalar (V2 a) = a
  scale s (V2 a b) = V2 (s*a) (s*b)
  zero = V2 0 0
  add = (+)

data V1 a = V1 !a
  deriving (Show, Eq)

instance Num a => Num (V1 a) where
  fromInteger = V1 <$> fromInteger
  (+) (V1 a) (V1 a') = V1 (a + a')
  (*) (V1 a) (V1 a') = V1 (a * a')

instance Num a => VectorSpace (V1 a) where
  type Scalar (V1 a) = a
  scale s (V1 a) = V1 (s*a)
  zero = V1 0
  add = (+)

{-
P(m,c | data) = 
  P(data | m,c) * P(m,c)
  --------------------------
            P(data)
-}
linear :: MonadMeasure m => [(Double, Double)] -> m (V3 Double)
linear input = do
  m <- normal 0 10
  c <- normal 0 10
  
  s <- normal 0 1
  
  -- P((x_i, y_i) | m,c) = normalPdf (m*x_i + c) s y_i
  -- -------------------------------------------------
  --              P((x_i, y_i)) = 1
  score . product $ fmap (\(x_i, y_i) -> normalPdf (m*x_i + c) (s**2) y_i) input
  
  pure $ V3 m c s

sampleWeighted :: (VectorSpace a, Scalar a ~ Double) => Measure a -> IO a
sampleWeighted = fmap (\(x, l) -> scale (ln (exp l)) x) . sampler . runWeightedT

mean :: (Foldable f, VectorSpace a, Scalar a ~ Double) => f a -> a    
mean xs = scale (1 / fromIntegral (length xs)) (foldl' add zero xs)

expectationMC :: (VectorSpace a, Scalar a ~ Double) => Int -> Measure a -> IO a
expectationMC samples m =
  fmap mean . replicateM samples $ sampleWeighted m

expectationMCMC :: (VectorSpace a, Scalar a ~ Double) => Int -> Measure a -> IO a
expectationMCMC samples m =
  fmap mean . sampler $ mcmc defaultMCMCConfig{numMCMCSteps = samples, numBurnIn = 0} m

{-
    5      3      9      3      8      4      7

A: 1/20 * 3/20 * 1/20 * 3/20 * 1/20 * 2/20 * 1/20 = 18/20^7
B: 2/20 * 2/20 * 1/20 * 2/20 * 2/20 * 2/20 * 2/20 = 64/20^7

18 / (18 + 64)
-}
itiala_exercise_3_1 :: MonadMeasure m => m Bool
itiala_exercise_3_1 = undefined
{-
  die <- bernoulli 0.5
  let
    roll =
      if die
      then categorical $ Vector.fromList [(6::Double)/20, 4/20, 3/20, 2/20, 1/20, 1/20, 1/20, 1/20, 1/20, 0]
      else categorical $ Vector.fromList [3/20, 3/20, 2/20, 2/20, 2/20, 2/20, 2/20, 2/20, 1/20, 1/20]
  
  for_ [5, 3, 9, 3, 8, 4, 7] $ \r ->
    condition . (r ==) . (1+) =<< roll
  
  -- rolls <- replicateM 7 roll
  -- condition $ fmap (+1) rolls == [5, 3, 9, 3, 8, 4, 7]
  
  pure die
-}

data Votes = Votes{ votesUp :: !Int, votesDown :: !Int }

simpleUserToUserTrust ::
  MonadDistribution m =>
  
  -- | How much have I upvoted and downvoted each user?
  Vector Votes ->
  
  -- | How much do I trust each user?
  --
  -- More specifically: for each user, the distribution over the probability
  -- that my next vote for them will be an upvote.
  --
  -- This is meant to evoke "my trust for you is the [my subjective]
  -- probability that you will do a good thing for me".
  Vector (m Double)
simpleUserToUserTrust =
  fmap (\(Votes ups downs) -> beta (fromIntegral ups + 1) (fromIntegral downs + 1))

transitiveUserToUserTrust ::
  MonadDistribution m =>

  -- | Simple user-to-user trust
  Vector (Vector (m Double)) ->

  -- | Transitive user-to-user trust
  --
  -- The distribution over probabilities that user A's next vote for user B will be
  -- an upvote.
  Vector (Vector (m Double))
transitiveUserToUserTrust simple =
  fmap
    (\simpleCol ->
      foldl' (Vector.zipWith _add) (Vector.replicate (Vector.length simple) distZero) $
      Vector.zipWith (\col scalar -> fmap (_scale scalar) col) simple simpleCol
    )
    simple
  where
    distZero :: MonadDistribution m => m Double
    distZero = _

main :: IO ()
main = pure ()

