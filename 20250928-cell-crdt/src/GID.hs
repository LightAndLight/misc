{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module GID where

import Base32 (bitsFromBase32, bitsToBase32, fromBits, toBits)
import Data.Binary (Binary)
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as ByteString
import Data.Word (Word16, Word8)
import GHC.Generics (Generic)
import System.Entropy (getEntropy)

data GID = GID !Word16 !Word16 !Word16 !Word16 !Word16
  deriving (Show, Eq, Ord, Generic, Binary)

newGID :: IO GID
newGID = do
  bytes <- ByteString.unpack <$> getEntropy 10
  pure $
    GID
      (word16FromBytes (bytes !! 0) (bytes !! 1))
      (word16FromBytes (bytes !! 2) (bytes !! 3))
      (word16FromBytes (bytes !! 4) (bytes !! 5))
      (word16FromBytes (bytes !! 6) (bytes !! 7))
      (word16FromBytes (bytes !! 8) (bytes !! 9))

word16FromBytes :: Word8 -> Word8 -> Word16
word16FromBytes a b = ((fromIntegral a :: Word16) `shiftL` 8) .|. fromIntegral b

gidToBase32 :: GID -> String
gidToBase32 (GID a b c d e) =
  bitsToBase32 $ toBits a ++ toBits b ++ toBits c ++ toBits d ++ toBits e

gidFromBase32 :: String -> Maybe GID
gidFromBase32 s
  | length s == 16 = do
      bits <- bitsFromBase32 s
      Just $
        GID
          (fromIntegral . fromBits $ take 16 bits)
          (fromIntegral . fromBits . take 16 $ drop 16 bits)
          (fromIntegral . fromBits . take 16 $ drop 32 bits)
          (fromIntegral . fromBits . take 16 $ drop 48 bits)
          (fromIntegral . fromBits . take 16 $ drop 64 bits)
  | otherwise = Nothing
