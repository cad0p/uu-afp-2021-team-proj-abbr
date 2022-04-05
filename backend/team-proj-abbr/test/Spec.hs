import           Test.Tasty

import           Test.LibCli.OutputInterface
    ( huOutputInterface
    , qcOutputInterface
    )
import           Test.LibCore.Decoder        (huDecoder, qcDecoder)
import           Test.LibCore.InputInterface
    ( huInputInterface
    , qcInputInterface
    )
import           Test.LibCore.KnowledgeBase  (huKnowledgeBase, qcKnowledgeBase)
import           Test.LibCore.Mapper         (huMapper, qcMapper)
import           Test.LibCore.Parser         (huParser, qcParser)


main :: IO ()
main = do
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

-- |Configuration of the QuickCheck tests
--
-- See: <https://hackage.haskell.org/package/QuickCheck>
properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup
  "QuickCheck"
  [ qcDecoder
  , qcInputInterface
  , qcKnowledgeBase
  , qcMapper
  , qcOutputInterface
  , qcParser
  ]


-- |Configuration of the HUnit tests
--
-- See: <https://hackage.haskell.org/package/HUnit>
unitTests :: TestTree
unitTests = testGroup "Unit tests" [hUnit]

hUnit :: TestTree
hUnit = testGroup
  "HUnit"
  [ huDecoder
  , huInputInterface
  , huKnowledgeBase
  , huMapper
  , huOutputInterface
  , huParser
  ]
