{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}

module Sound.Wave.Raw
  ( RawWaveData(..)
  , WaveRiffChunk(..)
  , WaveFmtChunk(..)
  , WaveDataChunk(..)
  ) where

data RawWaveData = RawWaveData
  { _rawWaveDataRiffChunk :: WaveRiffChunk
  , _rawWaveDataFmtChunk  :: WaveFmtChunk
  , _rawWaveDataDataChunk :: WaveDataChunk
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)

data WaveRiffChunk = WaveRiffChunk
  { _waveRiffChunkId     :: ByteString
  , _waveRiffChunkSize   :: Int
  , _waveRiffChunkFormat :: ByteString
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)

data WaveFmtChunk = WaveFmtChunk
  { _waveFmtChunkId          :: ByteString
  , _waveFmtChunkSize        :: Int
  , _waveFmtChunkAudioFormat :: Int
  , _waveFmtChunkNumChannels :: Int
  , _waveFmtChunkSampleRate  :: Int
  , _waveFmtChunkByteRate    :: Int
  , _waveFmtChunkBlockAlign  :: Int
  , _waveFmtBitsPerSample    :: Int
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)

data WaveDataChunk = WaveDataChunk
  { _waveDataChunkId   :: ByteString
  , _waveDataChunkSize :: Int
  , _waveDataChunkData :: ByteString
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)

