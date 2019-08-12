module Main
  ( main
  ) where

import Test.Tasty

tests :: TestTree
tests = testGroup "tests"
  [ testGroup "WAV encoding"
    [ testGroup "WAV sample encoding" []
    , testGroup "WAV data encoding" []
    , testGroup "WAV file encoding" []
    ]
  , testGroup "WAV decoding"
    [ testGroup "WAV sample decoding" []
    , testGroup "WAV data decoding" []
    , testGroup "WAV file decoding" []
    ]
  ]

main :: IO ()
main = defaultMain tests
