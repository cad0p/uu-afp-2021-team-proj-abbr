module Test.LibCore.Decoder (qcDecoder, huDecoder) where


import           Test.Tasty
import           Test.Tasty.HUnit

import           LibCore.Decoder


qcDecoder :: TestTree
qcDecoder = testGroup "Decoder" []

huDecoder :: TestTree
huDecoder = testGroup "Decoder" []
