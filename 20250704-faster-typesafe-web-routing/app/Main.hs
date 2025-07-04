{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad ((<=<))
import Lib
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  run 8080 <=< wai $ do
    get ("posts" // end) $ do
      _

    post ("posts" // end) $ do
      req <- askRequest
      _

    get ("posts" // param "id" @Int // end) $ \postId -> do
      _ postId

    put ("posts" // param "id" @Int // end) $ \postId -> do
      _ postId
