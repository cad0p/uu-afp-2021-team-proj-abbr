module Test.LibCli.Adapters
  ( qcAdapters
  , huAdapters
  ) where


import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.Map         as M
import qualified Data.Vector      as V
import           LibCli.Adapters
    ( KbEntry (KbEntry, abbreviation, expansion)
    , getKnowledgeBase
    , mapEntries
    )
import           LibCore.Models   (AKeyword (..), Keyword)



qcAdapters :: TestTree
qcAdapters = testGroup "Adapters" []

huAdapters :: TestTree
huAdapters = testGroup "Adapters" [huMapEntries, huGetKb]

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
      (  "(abbr) getKnowledgeBase from a vector of KbEntries produces"
      ++ " a Map of a list of pairs equivalent to those KbEntries"
      )
      (   getKnowledgeBase (V.fromList [exampleEntryAbbr])
      @?= M.fromList [examplePairAbbr]
      )
  ]
