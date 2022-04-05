module Test.LibCore.KnowledgeBase
  ( qcKnowledgeBase
  , huKnowledgeBase
  ) where


import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.Map              as M
import qualified Data.Vector           as V
import           LibCore.KnowledgeBase
import           LibCore.Models        (Keyword (Keyword, keyword, plural))



qcKnowledgeBase :: TestTree
qcKnowledgeBase = testGroup "KnowledgeBase" []

huKnowledgeBase :: TestTree
huKnowledgeBase = testGroup "KnowledgeBase" [huMapEntries, huGetKb]

exampleEntryAbbr :: KbEntry
exampleEntryAbbr =
  KbEntry { abbreviation = "abbr", expansion = "abbreviation" }

examplePairAbbr :: (Keyword, Keyword)
examplePairAbbr =
  ( Keyword { keyword = "abbr", plural = False }
  , Keyword { keyword = "abbreviation", plural = False }
  )


huMapEntries :: TestTree
huMapEntries = testGroup
  "mapEntries"
  [testCase "abbr" (mapEntries exampleEntryAbbr @?= examplePairAbbr)]

huGetKb :: TestTree
huGetKb = testGroup
  "huGetKb"
  [ testCase
      "1"
      (   getKnowledgeBase (V.fromList [exampleEntryAbbr])
      @?= M.fromList [examplePairAbbr]
      )
  ]
