{-# language DerivingStrategies #-}
{-# language LambdaCase #-}

-- | Types and functions for handling the compression/encoding format of a WAV
-- file.
module Sound.Wave.Encoding
  ( AudioFormat(..)
  , audioFormatCode
  , decodeAudioFormat
  ) where

import Data.Word (Word16)

-- | A bitstream encoding.
--
-- /Note/: Currently, this only supports linear pulse-code modulation.
data AudioFormat = PCM
  deriving stock (Eq, Show)

-- | Convert the audio format type to its encoding in the WAV file format
audioFormatCode :: AudioFormat -> Word16
audioFormatCode = \case
  PCM -> 1

-- | Parse the WAV file encoding of an audio format into the format it
-- represents.
decodeAudioFormat :: Word16 -> Maybe AudioFormat
decodeAudioFormat = \case
  1 -> Just PCM
  _ -> Nothing

