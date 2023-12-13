module SPWA.Supply (HasSupply (..), freshId) where

import Control.Monad.State.Class (MonadState, gets, modify)

class HasSupply s where
  getSupply :: s -> Int
  setSupply :: Int -> s -> s

freshId :: (MonadState s m, HasSupply s) => m String
freshId = do
  n <- gets getSupply
  modify $ setSupply (n + 1)
  pure $ show n