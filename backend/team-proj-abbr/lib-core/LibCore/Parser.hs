{-|
Description : The Parser interprets text into a structure of tokens based on our abbreviation syntax
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCore.Parser where

import           Data.Char          (isAlphaNum, isPunctuation, isSpace)
import           Data.Functor
import           LibCore.Models     (Keyword (Keyword), Token (DoMap, NoToken))
import           Text.Parsec
    ( ParseError
    , alphaNum
    , choice
    , many
    , many1
    , parse
    , satisfy
    , string
    , try
    )
import           Text.Parsec.String (Parser)

type ParseStructure = [Token]

-- | The abbreviation symbol. Can later be made configurable
abbSymbol :: String
abbSymbol = "@@"

-- | The plural symbol. String between the abbSymbol and the pluralSymbol are interpreted as plurals
pluralSymbol :: String
pluralSymbol = "'s"

-- | Given a string, parse it. This function can throw an error if parsing fails
doParse :: String -> ParseStructure
doParse s = case parseInput s of
  Left err -> error $ show err
  Right ps -> ps

-- | Map a string to a list of Tokens. For example:
-- >>> parseInput "@@bob"
-- Right [DoMap (Keyword {keyword = "bob", plural = False})]
-- >>> parseInput "@@fw's"
-- Right [DoMap (Keyword {keyword = "fw", plural = True})]
-- >>> parseInput "hello!"
-- Right [NoToken "hello!"]
parseInput :: String -> Either ParseError ParseStructure
parseInput = parse (mainParser abbSymbol pluralSymbol) ""

-- | The main parser tries to consume all input into a ParseStructure, given an
-- | abbreviation symbol and a plural symbol
mainParser :: String -> String -> Parser ParseStructure
mainParser s p = do many $ choice [spaceParser, try $ pluralAbbrParser s p, abbrParser s, punctuationParser, noAbbrParser]

-- | Inverse of the 'isSpace' function from Data.Char
notSpace :: Char -> Bool
notSpace c = not $ isSpace c

-- | Given an abbreviation string s and a plural end string p, parse the string between it
pluralAbbrParser :: String -> String -> Parser Token
pluralAbbrParser s p = do
  void $ string s
  a <- many1 alphaNum
  void $ string p
  return $ DoMap $ Keyword a True

-- | Given an abbreviation string s, parse the string after it
abbrParser :: String -> Parser Token
abbrParser s = do
  void $ string s
  a <- many1 $ satisfy isAlphaNum
  return $ DoMap $ Keyword a False

-- | Parse any string into a token
noAbbrParser :: Parser Token
noAbbrParser = noTokenParser notSpace

-- | Parser to process punctuation. We added this because abbreviations followed by
-- | punctuation without a space, such as 'hello!' were parsed as NoToken
punctuationParser :: Parser Token
punctuationParser = noTokenParser isPunctuation

-- | Parse spaces. We handle spaces separate because they are word delimiters
spaceParser :: Parser Token
spaceParser = noTokenParser isSpace

-- | Given a predicate, parse a character into a NoToken
noTokenParser :: (Char -> Bool) -> Parser Token
noTokenParser p = do
  a <- many1 $ satisfy p
  return $ NoToken a
