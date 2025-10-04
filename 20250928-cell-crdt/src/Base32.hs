{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Base32
  ( toBits
  , fromBits
  , bitsToBase32
  , bitsFromBase32
  )
where

import Data.Bits (FiniteBits, finiteBitSize)
import Data.List (elemIndex)
import Data.Word (Word8)
import Numeric.Natural (Natural)

toBits :: forall a. (FiniteBits a, Integral a) => a -> [Bool]
toBits = go (2 ^ (finiteBitSize (undefined :: a) - 1))
  where
    go 0 _ = []
    go !n !x =
      let (q, r) = quotRem x n
      in if q == 1 then True : go (n `div` 2) r else False : go (n `div` 2) r

fromBits :: [Bool] -> Natural
fromBits = snd . go
  where
    go [] = (0 :: Integer, 0)
    go (b : bs) =
      let
        (n, x) = go bs
        !x' = x + if b then 2 ^ n else 0
      in
        (n + 1, x')

base32Alphabet :: String
base32Alphabet = "abcdefghijklmnopqrstuvwxyz234567"

bitsToBase32 :: [Bool] -> String
bitsToBase32 = go
  where
    base32Char :: [Bool] -> Char
    base32Char bits = base32Alphabet !! fromIntegral (fromBits bits)

    go :: [Bool] -> String
    go [] = []
    go xs@(_ : _) =
      let (prefix, suffix) = splitAt 5 xs
      in let !char = base32Char prefix
         in char : go suffix

bitsFromBase32 :: String -> Maybe [Bool]
bitsFromBase32 s = do
  points <- traverse (`elemIndex` base32Alphabet) s
  pure
    [ bit
    | point <- points
    , bit <- drop 3 $ toBits (fromIntegral point :: Word8)
    ]
