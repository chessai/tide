{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

-- | Types for dealing with the channels in audio samples
module Sound.Wave.Channels
  ( Stereo(..)
  ) where

import GHC.Generics
import Data.Primitive.Array

import Sound.Wave.Sample

-- | Two channels of audio. Note that this will internally be represented as a
-- boxed array.
data Stereo a = Stereo
  { _stereoChan1 :: a
  , _stereoChan2 :: a
  }
  deriving stock (Eq, Ord, Show)
  deriving stock (Generic)

instance forall a. WaveSample a => WaveSample (Stereo a) where
  type SampleArr (Stereo a) = Array
  numChannels = 2 * numChannels @a
  bytesPerChannel = bytesPerChannel @a
  getSample = Stereo <$> getSample <*> getSample
  putSample (Stereo c1 c2) = putSample c1 <> putSample c2

