{-# lAnGuAgE ScopedTypeVariables #-}
{-# language AllowAmbiguousTypes #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language TypeApplications #-}

module Sound.Wave.Sample
  ( Stereo(..)
  , WaveSample(..)
  , sampleSize
  ) where

import Data.Word

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

class WaveSample d where
  -- | The number of channels in a sample
  numChannels :: Word16
  -- | The number of bytes per channel in a sample. Samples technically don't
  --   have to be divided into bits, but otherwise, it's possible to have
  --   non-integer block alignments (total number of bytes in a sample).
  bytesPerChannel :: Word16
  -- | Parse a single sample, in little-endian order
  getSample :: Get d
  -- | Turn a single sample into bytes, in little-endian order
  putSample :: d -> Put

-- | The size of a sample, in bytes
sampleSize :: forall d. WaveSample d => Word16
sampleSize = fromIntegral (numChannels @d) * fromIntegral (bytesPerChannel @d)

instance WaveSample Word8 where
  numChannels = 1
  bytesPerChannel = 1
  getSample = getWord8
  putSample = putWord8

instance WaveSample Float where
  numChannels = 1
  bytesPerChannel = 4
  getSample = getFloatle
  putSample = putFloatle

data Stereo a = Stereo
  { _stereoChan1 :: a
  , _stereoChan2 :: a
  }

instance WaveSample a => WaveSample (Stereo a) where
  numChannels = 2
  bytesPerChannel = 4
  getSample = Stereo <$> getSample <*> getSample
  putSample (Stereo c1 c2) = putSample c1 <> putSample c2

