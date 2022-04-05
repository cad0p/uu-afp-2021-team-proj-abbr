module Test.LibCore.KnowledgeBase
  ( qcKnowledgeBase
  , huKnowledgeBase
  ) where


import           Test.Tasty
import           Test.Tasty.HUnit

import           LibCore.KnowledgeBase


qcKnowledgeBase :: TestTree
qcKnowledgeBase = testGroup "KnowledgeBase" []

huKnowledgeBase :: TestTree
huKnowledgeBase = testGroup "KnowledgeBase" []
