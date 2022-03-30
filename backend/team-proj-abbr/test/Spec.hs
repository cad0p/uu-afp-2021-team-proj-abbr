
import           Test.Tasty

import           Test.LibCore.Decoder         (huDecoder, qcDecoder)
import           Test.LibCore.InputInterface
    ( huInputInterface
    , qcInputInterface
    )
import           Test.LibCore.KnowledgeBase   (huKnowledgeBase, qcKnowledgeBase)
import           Test.LibCore.Mapper          (huMapper, qcMapper)
import           Test.LibCore.OutputInterface
    ( huOutputInterface
    , qcOutputInterface
    )
import           Test.LibCore.Parser          (huParser, qcParser)


main :: IO ()
main = do
  defaultMain tests

tests       ::  TestTree
tests       =   testGroup "Tests"       [ properties, unitTests ]

properties  ::  TestTree
properties  =   testGroup "Properties"  [ qcProps ]

qcProps     ::  TestTree
qcProps     =   testGroup "QuickCheck"  [ qcDecoder
                                        , qcInputInterface
                                        , qcKnowledgeBase
                                        , qcMapper
                                        , qcOutputInterface
                                        , qcParser ]



unitTests   ::  TestTree
unitTests   =   testGroup "Unit tests"  [ hUnit ]

hUnit       ::  TestTree
hUnit       =   testGroup "HUnit"       [ huDecoder
                                        , huInputInterface
                                        , huKnowledgeBase
                                        , huMapper
                                        , huOutputInterface
                                        , huParser ]
