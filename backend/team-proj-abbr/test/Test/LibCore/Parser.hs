module Test.LibCore.Parser (qcParser, huParser) where


import           Test.Tasty
import           Test.Tasty.HUnit

import           LibCore.Parser


qcParser :: TestTree
qcParser = testGroup "Parser" []

huParser :: TestTree
huParser = testGroup "Parser" []
