module Main where

import Control.Monad (unless)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (execStateT, runStateT)
import Control.Monad.State.Class (get, modify)
import Control.Monad.Trans.Compose (getComposeT)
import Control.Monad.Writer (runWriterT)
import Control.Monad.Writer.Class (tell)
import Data.Bifunctor (first, second)
import Data.Functor.Identity (Identity (..))
import Lib

main :: IO ()
main = do
  do
    let results = runIdentity . collectListT . runExceptT . flip runStateT (0 :: Int) . getComposeT . runConcT $ do
          (_, s) <- concurrently (liftT $ modify (+ 2)) (liftT get)
          unless (s < 2) . liftT $ throwError "failure"
    print results

  do
    let results = runIdentity . collectListT . runWriterT . runConcT $ do
          _ <-
            liftT (tell "a")
              `concurrently` liftT (tell "b")
              `concurrently` liftT (tell "c")
          pure ()
    print results

  do
    let results = runIdentity . collectListT . flip execStateT (0 :: Int, 1 :: Int) . runConcT $ do
          _ <- concurrently (liftT $ modify $ first (+ 2)) (liftT $ modify $ second (* 3))
          pure ()
    print results