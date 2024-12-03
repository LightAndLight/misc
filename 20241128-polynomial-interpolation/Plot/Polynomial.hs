module Plot.Polynomial where

import Data.List (intercalate)

import Plot
import Polynomial

polyPlot :: (Num a, Eq a, Show a) => Polynomial a -> Plot
polyPlot = function . gnuplotPoly
 
gnuplotPoly :: (Num a, Eq a, Show a) => Polynomial a -> String
gnuplotPoly (Polynomial coeffs) =
  intercalate " + " $
  case coeffs' of
    [] -> [show 0]
    _ ->
      fmap
        (\(coeff, power) ->
          show coeff ++
          (if power == 0
            then ""
            else " * x" ++ (if power == 1 then "" else "**" ++ show power)
          )
        )
        coeffs'
  where
    coeffs' = filter ((0 /=) . fst) $ zip coeffs [0::Int ..]
