module Test.LibCore.Matcher (qcMatcher, huMatcher) where


import           Test.Tasty
import           Test.Tasty.HUnit

import           LibCore.Matcher


qcMatcher :: TestTree
qcMatcher = testGroup "Matcher" []

huMatcher :: TestTree
huMatcher = testGroup "Matcher" []
