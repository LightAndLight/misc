{-# LANGUAGE GADTs #-}
module Processing where

import Data.Function ((&))
import qualified System.Process as Process

data Cmd input output where
  Cmd :: String -> [String] -> Cmd () ()

cmd :: String -> [String] -> Cmd () ()
cmd = Cmd

exec :: Cmd input output -> input -> IO output
exec c input =
  case c of
    Cmd name args ->
      Process.callProcess name args
          
exec_ :: Cmd () output -> IO output
exec_ c = exec c ()

class RunResult result where
  configureResult :: Cmd input output -> Cmd input result

instance RunResult () where
  configureResult cmd@Cmd{} = cmd

run :: RunResult result => String -> [String] -> IO result
run name args = exec_ (cmd name args & configureResult)

examples :: IO ()
examples = do
  run "ls" []
