{-# language RecordWildCards #-}
{-# language FlexibleContexts #-}
{-# language DerivingStrategies #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}

module Sound.Wave
  ( module Wave
  , WaveData(..)
  , WaveException(..)
  , WaveFile(..)
  , decodeWaveFile
  , encodeWaveFile
  ) where

import Control.Arrow (left)
import Control.Monad
import Data.Int (Int64)

import qualified Data.Primitive.Contiguous as C
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Sound.Wave.Channels as Wave
import Sound.Wave.Encoding as Wave
import Sound.Wave.Sample as Wave

data WaveException
  = WaveParseException (BS.ByteString, Int64, Text)
  deriving stock (Eq, Show)

newtype WaveData d = WaveData { getWaveData :: SampleArr d d }

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

data WaveFile d = WaveFile
  { _waveFileAudioFormat :: AudioFormat
  , _waveFileSampleRate  :: !Word32
  , _waveFileData        :: WaveData d
  }

liftFailure :: (BL.ByteString, Int64, String) -> WaveException
liftFailure (src, off, msg) = WaveParseException (BL.toStrict src, off, T.pack msg)

encodeWaveFile :: WaveSample d => WaveFile d -> BL.ByteString
encodeWaveFile = encode

decodeWaveFile :: WaveSample d => BL.ByteString -> Either WaveException (WaveFile d)
decodeWaveFile = fmap (\(_, _, x) -> x) . left liftFailure . decodeOrFail

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

