{-# LANGUAGE GADTs #-}

module SPWA.MemoRef (MemoRef, newMemoRef, unsafeNewMemoRef, readMemoRef, setMemoRef) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

newtype MemoRef a = MemoRef (IORef (MemoMaybe a))

data MemoMaybe a where
  MemoNothing :: x -> MemoMaybe a
  MemoJust :: a -> MemoMaybe a

{-# NOINLINE unsafeNewMemoRef #-}
unsafeNewMemoRef ::
  -- | This argument isn't used by the function; it's here to stop what would
  -- be @unsafePerformIO newMemoRef@ getting let-floated.
  a ->
  MemoRef b
unsafeNewMemoRef a = MemoRef (unsafePerformIO $ newIORef (MemoNothing a))

{- | Do not use @unsafePerformIO newMemoRef@.
It will create a single IORef in global scope due to let-floating.
Use @unsafeNewMemoRef@ instead.
-}
newMemoRef :: IO (MemoRef a)
newMemoRef = MemoRef <$> newIORef (MemoNothing undefined)

readMemoRef :: MemoRef a -> IO (Maybe a)
readMemoRef (MemoRef ref) = do
  val <- readIORef ref
  case val of
    MemoNothing _ ->
      pure Nothing
    MemoJust a ->
      pure $ Just a

-- | Don't call this twice.
setMemoRef :: MemoRef a -> a -> IO ()
setMemoRef (MemoRef ref) a = writeIORef ref (MemoJust a)