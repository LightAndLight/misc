{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MyLib (
  -- * Monoid homomorphisms
  Mon,
  runMon,
  mid,
  mcomp,
  MPair,
  mpair,
  mfst,
  msnd,
  MSum,
  mleft,
  mright,
  msum,
  MZero,
  mterm,
  minit,
  mlift,

  -- * Buffered processing
  Split (..),
  inChunksOf,
  mapList,
  mapBytes,
  filterList,
  filterBytes,
  streamList,
  streamListM,
  streamBytesFromFile,
) where

import Control.Category (Category)
import qualified Control.Category
import Control.Monad.State.Class (get, put)
import Control.Monad.State.Strict (State, StateT)
import Control.Monad.Trans (lift)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Internal (mallocByteString)
import Data.Functor.Identity (Identity (..))
import qualified Data.List
import Data.Monoid (Ap (..))
import Data.Word (Word8)
import Foreign (ForeignPtr, Ptr, withForeignPtr)
import GHC.IO.Buffer (BufferState (ReadBuffer))
import System.IO (Handle, IOMode (..), hGetBuf, withFile)
import Prelude hiding (splitAt)

-- | The type of 'Monoid' homomorphisms.
newtype Mon a b = Mon {runMon :: a -> b}
  deriving (Category)

mid :: Mon a a
mid = Control.Category.id

mcomp :: Mon b c -> Mon a b -> Mon a c
mcomp = (Control.Category..)

-- | Products in the category of 'Monoid' homomorphisms.
data MPair a b = MPair a b
  deriving (Eq)

instance (Semigroup a, Semigroup b) => Semigroup (MPair a b) where
  MPair a b <> MPair a' b' = MPair (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (MPair a b) where
  mempty = MPair mempty mempty

mpair :: (Monoid x, Monoid a, Monoid b) => Mon x a -> Mon x b -> Mon x (a, b)
mpair (Mon f) (Mon g) = Mon (\x -> (f x, g x))

mfst :: (Monoid a, Monoid b) => Mon (a, b) a
mfst = Mon fst

msnd :: (Monoid a, Monoid b) => Mon (a, b) b
msnd = Mon snd

-- | Coproducts in the category of 'Monoid' homomorphisms.
newtype MSum a b = MSum [Either a b]

instance (Semigroup a, Semigroup b) => Semigroup (MSum a b) where
  MSum [] <> MSum ys = MSum ys
  MSum (x : xs) <> MSum ys =
    case x of
      Left a ->
        MSum $ reduceLeft a (xs <> ys)
      Right b ->
        MSum $ reduceRight b (xs <> ys)
   where
    reduceLeft a [] = [Left a]
    reduceLeft a (Left a' : zs) =
      reduceLeft (a <> a') zs
    reduceLeft a (Right b : zs) =
      Left a : reduceRight b zs

    reduceRight b [] = [Right b]
    reduceRight b (Right b' : zs) =
      reduceRight (b <> b') zs
    reduceRight b (Left a : zs) =
      Right b : reduceLeft a zs

instance (Eq a, Eq b, Monoid a, Monoid b) => Eq (MSum a b) where
  MSum [] == MSum [] = True
  MSum (Left a : xs) == ys | a == mempty = MSum xs == ys
  MSum (Right b : xs) == ys | b == mempty = MSum xs == ys
  xs == MSum (Left a : ys) | a == mempty = xs == MSum ys
  xs == MSum (Right b : ys) | b == mempty = xs == MSum ys
  MSum (Left a : xs) == MSum (Left a' : ys) = a == a' && MSum xs == MSum ys
  MSum (Left{} : _) == MSum (Right{} : _) = False
  MSum (Right{} : _) == MSum (Left{} : _) = False
  MSum (Right b : xs) == MSum (Right b' : ys) = b == b' && MSum xs == MSum ys
  _ == _ = False

mleft :: (Monoid a, Monoid b) => Mon a (MSum a b)
mleft = Mon $ \a -> MSum [Left a]

mright :: (Monoid a, Monoid b) => Mon b (MSum a b)
mright = Mon $ \b -> MSum [Right b]

msum :: (Monoid a, Monoid b, Monoid x) => Mon a x -> Mon b x -> Mon (MSum a b) x
msum (Mon f) (Mon g) = Mon (\(MSum xs) -> foldMap (either f g) xs)

-- | The initial and terminal object in the category of 'Monoid' homomorphisms.
data MZero = MZero

instance Semigroup MZero where
  MZero <> MZero = MZero

mterm :: (Monoid x) => Mon x MZero
mterm = Mon (const MZero)

minit :: (Monoid x) => Mon MZero x
minit = Mon (\MZero -> mempty)

mlift :: (Monoid m) => (a -> m) -> Mon [a] m
mlift f = Mon (foldMap f)

class Split s where
  size :: s -> Int
  splitAt :: Int -> s -> Either s (s, s)

  chunksOf :: Int -> s -> [s]
  chunksOf n xs
    | n <= 0 = undefined
    | otherwise = go xs
   where
    go :: s -> [s]
    go s =
      case splitAt n s of
        Left s' ->
          if size s' == 0 then [] else [s']
        Right (s', s'') ->
          s' : go s''

instance Split [a] where
  size = length
  splitAt n s =
    let (prefix, suffix) = Data.List.splitAt n s
     in if null suffix
          then Left prefix
          else Right (prefix, suffix)

instance Split ByteString where
  size = ByteString.length
  splitAt n s =
    let (prefix, suffix) = ByteString.splitAt n s
     in if ByteString.null suffix
          then Left prefix
          else Right (prefix, suffix)

inChunksOf :: (Split s, Monoid m) => Int -> Mon s m -> s -> m
inChunksOf n f = foldMap (runMon f) . chunksOf n

{- |
@
'fmap' f [] = []
'fmap' f (a '<>' b) = 'fmap' f a '<>' 'fmap' f b
@
-}
mapList :: (a -> b) -> Mon [a] [b]
mapList f = Mon (fmap f)

{- |
@
'ByteString.map' f 'mempty' = 'mempty'
'ByteString.map' f (a '<>' b) = 'ByteString.map' f a '<>' 'ByteString.map' f b
@
-}
mapBytes :: (Word8 -> Word8) -> Mon ByteString ByteString
mapBytes f = Mon (ByteString.map f)

{- |
@
'filter' f [] = []
'filter' f (a '<>' b) = 'filter' f a '<>' 'filter' f b
@
-}
filterList :: (a -> Bool) -> Mon [a] [a]
filterList f = Mon (filter f)

{- |
@
'ByteString.filter' f 'mempty' = 'mempty'
'ByteString.filter' f (a '<>' b) = 'ByteString.filter' f a '<>' 'ByteString.filter' f b
@
-}
filterBytes :: (Word8 -> Bool) -> Mon ByteString ByteString
filterBytes f = Mon (ByteString.filter f)

data Result s a = Yield s a | Skip s | Done
  deriving (Eq, Show, Functor)

streamList :: (s -> a -> Result s b) -> Mon [a] (Ap (State s) [b])
streamList f = streamListM (\s a -> Identity (f s a))

streamListM :: forall m s a b. (Monad m) => (s -> a -> m (Result s b)) -> Mon [a] (Ap (StateT s m) [b])
streamListM f =
  mlift $ \a -> Ap $ do
    s <- get
    result <- lift $ f s a
    case result of
      Yield s' b -> do
        put s'
        pure [b]
      Skip s' -> do
        put s'
        pure []
      Done ->
        pure []

streamBytesFromFile :: (Monoid m) => FilePath -> Mon ByteString m -> IO m
streamBytesFromFile path (Mon f) = do
  withFile path ReadMode $ \handle -> do
    fptr :: ForeignPtr Word8 <- mallocByteString 1024
    withForeignPtr fptr $ \ptr -> go mempty handle ptr
 where
  go :: (Monoid m) => m -> Handle -> Ptr Word8 -> IO m
  go !m handle ptr = do
    readCount <- hGetBuf handle ptr bufferSize
    if readCount < bufferSize
      then do
        -- The monoid homomorphism is a function `ByteString -> m`, but to call it I'd have to
        -- allocate a `ByteString`. I need the `g : Word8 -> m` that gives rise to the `ByteString -> m`
        -- via `foldMap g . ByteString.unpack`.
        m' <- _ ptr readCount f
        pure $ m <> m'
      else do
        m' <- _ ptr readCount f
        go (m <> m') handle ptr

  bufferSize = 1024