{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (throwIO)
import Lib
import Network.Wai.Handler.Warp (run)
import Data.Function ((&))
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Encoding as Text.Encoding

main :: IO ()
main = do
  checked <- either throwIO pure $ checkRoutes routes
  print $ sitemap $ getCheckedRoutes checked
  run 8080 =<< waiRoutes routes

routes :: Routes (HandlerT IO) ()
routes = do
  get ("posts" // end) $ do
    pure undefined

  post ("posts" // end) $ do
    fields <- body formUrlEncoded
    let postId = 1 :: Int
    let location = "/posts/" <> printParam postId
    pure $
      response
        & withStatus 303
        & withHeaders [("Location", Text.Encoding.encodeUtf8 location)]
        & withBody text ("redirecting to " <> LazyText.fromStrict location)

  get ("posts" // param "id" @Int // end) $ \postId -> do
    undefined postId

  put ("posts" // param "id" @Int // end) $ \postId -> do
    undefined postId
