module Polynomial where

import Zip (zipWithDefault)

newtype Polynomial a = Polynomial [a]
  deriving Show
      
runPoly :: Num a => Polynomial a -> a -> a
runPoly (Polynomial coeffs) x =
  sum $ zipWith (\power coeff -> coeff * x^power) [0::Int ..] coeffs

instance Num a => Num (Polynomial a) where
  fromInteger x = term (fromInteger x) 0

  negate (Polynomial a) = Polynomial (fmap negate a)

  Polynomial a + Polynomial b = Polynomial (zipWithDefault 0 (+) a b)

  Polynomial a * Polynomial b =
    sum $ do
      (power, coeff) <- zip [0..] a
      (power', coeff') <- zip [0..] b
      pure $ term (coeff * coeff') (power + power')

zeroPoly :: Num a => Polynomial a
zeroPoly = Polynomial [0]

scalePoly :: Num a => a -> Polynomial a -> Polynomial a
scalePoly s (Polynomial xs) = Polynomial $ fmap (s *) xs

term :: Num a => a -> Int -> Polynomial a
term a n = Polynomial (replicate n 0 ++ [a])

newtype Term a = Term (Polynomial a)
  deriving Num

val :: Num a => a -> Term a
val n = Term (term n 0)

poly :: Num a => (Term a -> Term a) -> Polynomial a
poly f = let Term poly = f (Term (term 1 1)) in poly
