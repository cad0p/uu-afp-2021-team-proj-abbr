module Test.LibCore.OutputInterface (qcOutputInterface, huOutputInterface) where


import           Test.Tasty
import           Test.Tasty.HUnit

import           LibCore.OutputInterface


qcOutputInterface :: TestTree
qcOutputInterface = testGroup "OutputInterface" []

huOutputInterface :: TestTree
huOutputInterface = testGroup "OutputInterface" []
