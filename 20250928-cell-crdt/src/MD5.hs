{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module MD5
  ( MD5 (..)
  , hashMD5
  , md5ToBase32
  , md5FromBase32
  )
where

import Base32 (bitsFromBase32, bitsToBase32, fromBits, toBits)
import qualified Crypto.Hash.MD5 as MD5
import Data.Binary (Binary)
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as ByteString
import Data.ByteString.Lazy (LazyByteString)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)

word64FromBytes ::
  Word8 ->
  Word8 ->
  Word8 ->
  Word8 ->
  Word8 ->
  Word8 ->
  Word8 ->
  Word8 ->
  Word64
word64FromBytes a b c d e f g h =
  fromIntegral a `shiftL` 56
    .|. fromIntegral b `shiftL` 48
    .|. fromIntegral c `shiftL` 40
    .|. fromIntegral d `shiftL` 32
    .|. fromIntegral e `shiftL` 24
    .|. fromIntegral f `shiftL` 16
    .|. fromIntegral g `shiftL` 8
    .|. fromIntegral h

data MD5 = MD5 !Word64 !Word64
  deriving (Show, Eq, Ord, Generic, Binary)

hashMD5 :: LazyByteString -> MD5
hashMD5 input =
  MD5
    ( word64FromBytes
        (bytes !! 0)
        (bytes !! 1)
        (bytes !! 2)
        (bytes !! 3)
        (bytes !! 4)
        (bytes !! 5)
        (bytes !! 6)
        (bytes !! 7)
    )
    ( word64FromBytes
        (bytes !! 8)
        (bytes !! 9)
        (bytes !! 10)
        (bytes !! 11)
        (bytes !! 12)
        (bytes !! 13)
        (bytes !! 14)
        (bytes !! 15)
    )
  where
    bytes = ByteString.unpack . MD5.finalize $ MD5.startlazy input

md5ToBase32 :: MD5 -> String
md5ToBase32 (MD5 a b) = bitsToBase32 $ toBits a ++ toBits b ++ [False, False]

md5FromBase32 :: String -> Maybe MD5
md5FromBase32 s
  | length s == 26 = do
      bits <- bitsFromBase32 s
      pure $
        MD5
          ( word64FromBytes
              (fromIntegral . fromBits $ take 8 bits)
              (fromIntegral . fromBits . take 8 $ drop 8 bits)
              (fromIntegral . fromBits . take 8 $ drop 16 bits)
              (fromIntegral . fromBits . take 8 $ drop 24 bits)
              (fromIntegral . fromBits . take 8 $ drop 32 bits)
              (fromIntegral . fromBits . take 8 $ drop 40 bits)
              (fromIntegral . fromBits . take 8 $ drop 48 bits)
              (fromIntegral . fromBits . take 8 $ drop 56 bits)
          )
          ( word64FromBytes
              (fromIntegral . fromBits . take 8 $ drop 64 bits)
              (fromIntegral . fromBits . take 8 $ drop 72 bits)
              (fromIntegral . fromBits . take 8 $ drop 80 bits)
              (fromIntegral . fromBits . take 8 $ drop 88 bits)
              (fromIntegral . fromBits . take 8 $ drop 96 bits)
              (fromIntegral . fromBits . take 8 $ drop 104 bits)
              (fromIntegral . fromBits . take 8 $ drop 112 bits)
              (fromIntegral . fromBits . take 8 $ drop 120 bits)
          )
  | otherwise = Nothing
