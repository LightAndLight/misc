module Plot where

import Data.Maybe (catMaybes)
import Data.List (intercalate)
import System.Process (callProcess, readProcess)

data Plot
  = Plot
  { plotCommand :: String
  , plotData :: [String]
  , plotXRange :: Maybe (Double, Double)
  , plotYRange :: Maybe (Double, Double)
  }
  deriving Show

function :: String -> Plot
function fn = Plot fn [] Nothing Nothing

points :: [(Double, Double)] -> Plot
points ps =
  Plot
    "\"-\" using 1:2:(sprintf(\"(%g,%g)\", $1, $2)) with labels offset 0,0.5 point pt 7 ps 2 notitle"
    (fmap (\(x, y) -> show x ++ " " ++ show y) ps ++ ["e"])
    (Just (minimum (fmap fst ps), maximum (fmap fst ps)))
    (Just (minimum (fmap snd ps), maximum (fmap snd ps)))

plots :: [Plot] -> IO ()
plots ps = do
  _ <- readProcess "gnuplot" ["-e", plotsCommands ps] (unlines points)
  pure ()
  where
    points = foldMap plotData ps

plotsXMinHint :: [Plot] -> Maybe Double
plotsXMinHint ps =
  case catMaybes $ fmap (fmap fst . plotXRange) ps of
    [] -> Nothing
    xs -> Just $ minimum xs

plotsXMaxHint :: [Plot] -> Maybe Double
plotsXMaxHint ps =
  case catMaybes $ fmap (fmap snd . plotXRange) ps of
    [] -> Nothing
    xs -> Just $ maximum xs

plotsYMinHint :: [Plot] -> Maybe Double
plotsYMinHint ps =
  case catMaybes $ fmap (fmap fst . plotYRange) ps of
    [] -> Nothing
    xs -> Just $ minimum xs

plotsYMaxHint :: [Plot] -> Maybe Double
plotsYMaxHint ps =
  case catMaybes $ fmap (fmap snd . plotYRange) ps of
    [] -> Nothing
    xs -> Just $ maximum xs

paddingCoeff :: Double
paddingCoeff = 0.2

rangeFromHints :: (Maybe Double, Maybe Double) -> (String, String)
rangeFromHints (minHint, maxHint) =
  case (minHint, maxHint) of
    (Nothing, Nothing) ->
      ("0", "")
    (Nothing, Just x') ->
      if x' > 0
      then (show 0, show $ x' + paddingCoeff * x')
      else ("", show x')
    (Just x, Nothing) ->
      if x < 0
      then (show x, show 0)
      else (show 0, "")
    (Just x, Just x') 
      | x == x' -> (show $ x - 1, show $ x + 1)
      | otherwise ->
          let paddedMin = x - paddingCoeff * (x' - x) in
          if paddedMin > 0
          then (show 0, show $ x' + paddingCoeff * x')
          else (show paddedMin, show $ x' + paddingCoeff * (x' - x))

plotsCommands :: [Plot] -> String
plotsCommands ps = unlines $ before ++ [commands] ++ after
  where
    before =
      [ "set terminal qt linewidth 2;"
      , "set xlabel \"X axis\";"
      , "set ylabel \"Y axis\";"
      , "set xrange [" <> xmin <> ":" <> xmax <> "];"
      , "set yrange [" <> ymin <> ":" <> ymax <> "];"
      ]

    commands = "plot " ++ intercalate ", " (fmap plotCommand ps) ++ ";"

    after =
      [ "pause mouse"
      ]

    xminHint, xmaxHint :: Maybe Double
    xminHint = plotsXMinHint ps
    xmaxHint = plotsXMaxHint ps

    yminHint, ymaxHint :: Maybe Double
    yminHint = plotsYMinHint ps
    ymaxHint = plotsYMaxHint ps
    
    xmin, xmax :: String
    (xmin, xmax) = rangeFromHints (xminHint, xmaxHint)

    ymin, ymax :: String
    (ymin, ymax) = rangeFromHints (yminHint, ymaxHint)

plot :: Plot -> IO ()
plot p = plots [p]
