module Web.Warp where

import Barbies (ApplicativeB, TraversableB)
import qualified Network.Wai.Handler.Warp as Warp
import Web.Endpoint (Endpoint)
import Web.Route (Route)
import Web.Wai (toApplicationUnchecked)

serve :: (TraversableB b, ApplicativeB b) => Int -> b Route -> b (Endpoint IO) -> IO ()
serve port routes endpoints = do
  putStrLn $ "Server running on port " <> show port
  Warp.run port app
  where
    app = toApplicationUnchecked routes endpoints
