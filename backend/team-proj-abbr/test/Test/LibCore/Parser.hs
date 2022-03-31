module Test.LibCore.Parser (qcParser, huParser) where


import           Test.Tasty
import           Test.Tasty.HUnit

import           LibCore.Models
    ( Keyword (Keyword, Plural)
    , Token (DoMap, NoToken)
    )
import           LibCore.Parser   (doParse)


qcParser :: TestTree
qcParser = testGroup "Parser" []

huParser :: TestTree
huParser = testGroup "Parser" [parseNoToken, parseKeyword, parsePlural, parsePluralWord]

parseNoToken :: TestTree
parseNoToken = testCase "Test the parsing of a NoToken" $ assertEqual "" [NoToken "hello"] (doParse "hello")

parseKeyword :: TestTree
parseKeyword = testCase "Test the parsing of a Keyword" $ assertEqual "" [DoMap $ Keyword "hello"] (doParse "@@hello")

parsePlural :: TestTree
parsePlural = testCase "Test the parsing of a Plural" $ assertEqual "" [DoMap $ Plural "hello"] (doParse "@@hello's")

parsePluralWord :: TestTree
parsePluralWord = testCase "Test that a plural word is not parsed as plural" $ assertEqual "" [NoToken "hello's"] (doParse "hello's")
