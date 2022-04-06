module Test.LibCore.Parser (qcParser, huParser) where


import           Test.Tasty
import           Test.Tasty.HUnit

import           LibCore.Models   (Keyword (Keyword), Token (DoMap, NoToken))
import           LibCore.Parser   (doParse)


qcParser :: TestTree
qcParser = testGroup "Parser" []

huParser :: TestTree
huParser = testGroup "Parser" [parseNoToken, parseKeyword, parsePlural, parsePluralWord, parsePunctuation, parseSpacing]

parseNoToken :: TestTree
parseNoToken = testCase "Test the parsing of a NoToken" $ assertEqual "" [NoToken "hello"] (doParse "hello")

parseKeyword :: TestTree
parseKeyword = testCase "Test the parsing of a Keyword" $ assertEqual "" [DoMap $ Keyword "hello" False] (doParse "@@hello")

parsePlural :: TestTree
parsePlural = testCase "Test the parsing of a Plural" $ assertEqual "" [DoMap $ Keyword "hello" True] (doParse "@@hello's")

parsePluralWord :: TestTree
parsePluralWord = testCase "Test that a plural word is not parsed as plural" $ assertEqual "" [NoToken "hello's"] (doParse "hello's")

parsePunctuation :: TestTree
parsePunctuation = testCase "Test that an abbreviation with punctuation is parsed correctly" $ assertEqual "" [DoMap $ Keyword "hello" False, NoToken "!"] (doParse "@@hello!")

parseSpacing :: TestTree
parseSpacing = testCase "Test the parsing of spacing" $ assertEqual "" [NoToken " \n\t\r"] (doParse " \n\t\r")
