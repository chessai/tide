{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}

-- | Functions for parsing and encoding WAV files with custom audio sample
-- formats.
module Sound.Wave
  ( decodeWaveFile
  , dumpWaveFile
  , encodeWaveFile
  , parseWaveFile
  , WaveFile(..)
  , WaveData(..)
  , WaveException(..)
  , AudioFormat(..)
  , module Sound.Wave.Channels
  , module Sound.Wave.Sample
  ) where

import Control.Arrow (left)
import Control.Monad
import Data.Int (Int64)
import Control.Exception
import GHC.Generics

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Primitive.Contiguous as C
import qualified Data.Text as T

import Sound.Wave.Channels
import Sound.Wave.Encoding
import Sound.Wave.Sample

--------------------------------------------------------------------------------

-- | Turn a WAV file into its 'BL.ByteString' representation
encodeWaveFile :: WaveSample d => WaveFile d -> BL.ByteString
{-# INLINE encodeWaveFile #-}
encodeWaveFile = encode

-- | Parse a 'BL.ByteString' into the WAV file it represents, or return a 'Left
-- WaveException' is it failed. Note that this expects you to know the
-- representation of the samples in advance.
decodeWaveFile :: WaveSample d => BL.ByteString -> Either WaveException (WaveFile d)
decodeWaveFile = fmap (\(_, _, x) -> x) . left liftFailure . decodeOrFail
  where
    liftFailure (src, off, msg) = WaveParseException (BL.toStrict src, off, T.pack msg)

-- | Like 'encodeWaveFile', but write to the provided 'FilePath'.
dumpWaveFile :: WaveSample d => FilePath -> WaveFile d -> IO ()
{-# INLINE dumpWaveFile #-}
dumpWaveFile path = BL.writeFile path . encode

-- | Like 'decodeWaveFile', but read from the provided 'FilePath'.
parseWaveFile :: WaveSample d => FilePath -> IO (Either WaveException (WaveFile d))
{-# INLINE parseWaveFile #-}
parseWaveFile = fmap decodeWaveFile . BL.readFile

--------------------------------------------------------------------------------

-- | An exception that occurred during WAV decoding.
data WaveException
  = WaveParseException (BS.ByteString, Int64, Text)
  deriving stock (Eq, Show)
  deriving stock (Generic)

instance Exception WaveException

-- | A contiguous array of the audio samples from the data chunk. Note that the
-- array representation is polymorphic with respect to its element type.
newtype WaveData d = WaveData { getWaveData :: SampleArr d d }

deriving newtype instance (Eq d,   Eq   (SampleArr d d)) => Eq   (WaveData d)
deriving newtype instance (Ord d,  Ord  (SampleArr d d)) => Ord  (WaveData d)
deriving newtype instance (Show d, Show (SampleArr d d)) => Show (WaveData d)

-- | This instance accounts for the leading metadata in the data chunk
instance forall d. WaveSample d => Binary (WaveData d) where
  get = do
    void $ getWord32be
    size <- getWord32le
    let sampleBytes = fromIntegral $ sampleSize @d
    when (size `mod` sampleBytes /= 0) $ do
      fail "Data size is not divisible by sample size"
    let len = fromIntegral (size `div` sampleBytes)
    fmap (WaveData . C.fromListN len) $ replicateM len (getSample @d)

  put (WaveData arr) = do
    putByteString "data"
    let len = fromIntegral $ C.size arr
        size = sampleSize @d * len
    putWord32le (fromIntegral size)
    C.foldMap (putSample @d) arr

--------------------------------------------------------------------------------

-- | The WAV file itself. The only parts of the RIFF/format headers that aren't
-- already encoded by the sample type are the audio format and sample rate.
--
-- /Note:/ Currently, only an uncompressed PCM audio format is supported.
data WaveFile d = WaveFile
  { _waveFileAudioFormat :: AudioFormat
  , _waveFileSampleRate  :: !Word32
  , _waveFileData        :: WaveData d
  }

deriving stock instance (Eq d,   Eq   (SampleArr d d)) => Eq   (WaveFile d)
deriving stock instance (Show d, Show (SampleArr d d)) => Show (WaveFile d)

instance WaveSample d => Binary (WaveFile d) where
  put = putWaveFile
  get = getWaveFile

putWaveFile :: forall d. WaveSample d => WaveFile d -> Put
putWaveFile WaveFile{..} = do
  -- RIFF header
  putByteString "RIFF"
  let fmtChunkSize = 8 + 16
      dataLen = fromIntegral $ C.size $ getWaveData _waveFileData
      dataSize = sampleSize @d * dataLen
      dataChunkSize = 8 + dataSize
      chunkSize = 4 + fmtChunkSize + dataChunkSize
  putWord32le (fromIntegral chunkSize)
  putByteString "WAVE"
  -- "fmt " subchunk
  putByteString "fmt "
  putWord32le 16
  -- AudioFormat
  putWord16le $ audioFormatCode _waveFileAudioFormat
  -- NumChannels
  putWord16le $ numChannels @d
  -- SampleRate
  putWord32le _waveFileSampleRate
  -- ByteRate = SampleRate * NumChannels * BitsPerSample/8
  putWord32le $ _waveFileSampleRate * fromIntegral (sampleSize @d)
  -- BlockAlign = NumChannels * BitsPerSample/8
  putWord16le $ sampleSize @d
  -- BitsPerSample
  putWord16le $ 8 * bytesPerChannel @d
  -- "data" subchunk
  put _waveFileData

getWaveFile :: forall d. WaveSample d => Get (WaveFile d)
getWaveFile = do
  let checkId expected chunkType = do
        chunkId <- getByteString 4
        when (chunkId /= expected) $ do
          fail $ "Unsupported " <> chunkType <> ": " <> BC8.unpack chunkId
      assertValue desc got expected = do
        when (got /= expected) $ fail $ desc <> ": expected " <> show expected <> ", got " <> show got
  checkId "RIFF" "chunk ID"
  void getWord32le
  checkId "WAVE" "format"
  -- "fmt " subchunk
  checkId "fmt " "subchunk ID"
  fmtChunkSize <- getWord32le
  when (fmtChunkSize /= 16) $ do
    fail $ "Unexpected size of fmt chunk: " <> show fmtChunkSize <> ". Non-PCM formats are currently not supported."
  audioFormat <- maybe (fail "Encountered unsupported non-PCM format.") pure . decodeAudioFormat =<< getWord16le
  channels <- getWord16le
  assertValue "Channels" channels (numChannels @d)
  sampleRate <- getWord32le
  byteRate <- getWord32le
  assertValue "Byte rate" byteRate (sampleRate * fromIntegral (sampleSize @d))
  blockAlign <- getWord16le
  assertValue "Block align" blockAlign (sampleSize @d)
  bitsPerSample <- getWord16le
  assertValue "Bits per sample" bitsPerSample (8 * bytesPerChannel @d)
  waveData <- get @(WaveData d)
  pure $ WaveFile
    { _waveFileAudioFormat = audioFormat
    , _waveFileSampleRate  = sampleRate
    , _waveFileData        = waveData
    }

