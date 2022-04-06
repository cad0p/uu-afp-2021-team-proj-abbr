module Test.LibCore.Models
  ( qcModels
  , huModels
  ) where


import           Test.Tasty
import           Test.Tasty.HUnit

import           LibCore.Models


qcModels :: TestTree
qcModels = testGroup "Models" []

huModels :: TestTree
huModels = testGroup "Models" []
