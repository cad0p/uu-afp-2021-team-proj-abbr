module Test.LibCore.Mapper (qcMapper, huMapper) where


import           Test.Tasty
import           Test.Tasty.HUnit

import           LibCore.Mapper


qcMapper :: TestTree
qcMapper = testGroup "Mapper" []

huMapper :: TestTree
huMapper = testGroup "Mapper" []
