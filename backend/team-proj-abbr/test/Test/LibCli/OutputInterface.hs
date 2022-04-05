module Test.LibCli.OutputInterface
  ( qcOutputInterface
  , huOutputInterface
  ) where


import           Test.Tasty
import           Test.Tasty.HUnit

import           LibCli.OutputInterface


qcOutputInterface :: TestTree
qcOutputInterface = testGroup "OutputInterface" []

huOutputInterface :: TestTree
huOutputInterface = testGroup "OutputInterface" []
