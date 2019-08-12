{-# language DerivingStrategies #-}
{-# language LambdaCase #-}

module Sound.Wave.Encoding
  ( AudioFormat(..)
  , audioFormatCode
  , decodeAudioFormat
  ) where

import Data.Word (Word16)

-- | A bitstream encoding. Currently, this only unencoded linear pulse-code
--   modulation.
data AudioFormat = PCM
  deriving stock (Eq, Show)

audioFormatCode :: AudioFormat -> Word16
audioFormatCode = \case
  PCM -> 1

decodeAudioFormat :: Word16 -> Maybe AudioFormat
decodeAudioFormat = \case
  1 -> Just PCM
  _ -> Nothing

