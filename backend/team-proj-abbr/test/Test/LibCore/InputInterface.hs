module Test.LibCore.InputInterface (qcInputInterface, huInputInterface) where


import           Test.Tasty
import           Test.Tasty.HUnit

import           LibCore.InputInterface


qcInputInterface :: TestTree
qcInputInterface = testGroup "InputInterface" []

huInputInterface :: TestTree
huInputInterface = testGroup "InputInterface" []
