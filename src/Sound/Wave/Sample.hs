{-# language AllowAmbiguousTypes #-}
{-# language FlexibleContexts #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Sound.Wave.Sample
  ( WaveSample(..)
  , sampleSize
  ) where

import Data.Kind
import Data.Word

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Primitive.Contiguous

class (Contiguous (SampleArr d), Element (SampleArr d) d) => WaveSample d where
  -- | An array representation for the sample type
  type SampleArr d :: Type -> Type
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
  type SampleArr Word8 = PrimArray
  numChannels = 1
  bytesPerChannel = 1
  getSample = getWord8
  putSample = putWord8

instance WaveSample Word16 where
  type SampleArr Word16 = PrimArray
  numChannels = 1
  bytesPerChannel = 2
  getSample = getWord16le
  putSample = putWord16le

instance WaveSample Word32 where
  type SampleArr Word32 = PrimArray
  numChannels = 1
  bytesPerChannel = 4
  getSample = getWord32le
  putSample = putWord32le

instance WaveSample Float where
  type SampleArr Float = PrimArray
  numChannels = 1
  bytesPerChannel = 4
  getSample = getFloatle
  putSample = putFloatle

instance WaveSample Double where
  type SampleArr Double = PrimArray
  numChannels = 1
  bytesPerChannel = 8
  getSample = getDoublele
  putSample = putDoublele

