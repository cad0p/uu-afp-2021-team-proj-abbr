module LibCore.Parser where

import           Data.Functor
import           LibCore.Models
    ( Keyword (Keyword, Plural)
    , Token (DoMap, NoToken)
    )
import           Text.Parsec
    ( ParseError
    , alphaNum
    , choice
    , many
    , many1
    , parse
    , spaces
    , string
    , try
    )
import           Text.Parsec.String (Parser)

type ParseStructure
  = [Token]

-- | The abbreviation symbol. Can later be made configurable
abbSymbol :: String
abbSymbol = "@@"

-- | The plural symbol. String between the abbSymbol and the pluralSymbol are interpreted as plurals
pluralSymbol :: String
pluralSymbol = "'s"

-- | Consume whitespace characters
whitespace :: Parser ()
whitespace = void spaces

-- | Lexeme parsers consume the whitespace after them
lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

-- | Given a string, parse it. This function can throw an error if parsing fails
doParse :: String -> ParseStructure
doParse s = case parseInput s of
  Left err -> error $ show err
  Right ps -> ps

-- | Map a string to a list of Tokens. For example:
-- >>> parseInput "@@bob"
-- Right [DoMap (Keyword "bob")]
-- >>> parseInput "@@fw's"
-- Right [DoMap (Plural "fw")]
-- >>> parseInput "hello!"
-- Right [NoToken "hello"]
parseInput :: String -> Either ParseError ParseStructure
parseInput = parse (mainParser abbSymbol pluralSymbol) ""

-- | The main parser tries to consume all input into a ParseStructure, given an
-- | abbreviation symbol and a plural symbol
mainParser :: String -> String -> Parser ParseStructure
mainParser s p = do many $ choice [try $lexeme $ pluralAbbrParser s p, lexeme $ abbrParser s, lexeme noAbbrParser]

-- | Given an abbreviation string s and a plural end string p, parse the string between it
pluralAbbrParser :: String -> String -> Parser Token
pluralAbbrParser s p = do
  void $ string s
  a <- many1 alphaNum
  void $ string p
  return $ DoMap $ Plural a

-- | Given an abbreviation string s, parse the string after it
abbrParser :: String -> Parser Token
abbrParser s = do
  void $ string s
  a <- many1 alphaNum
  return $ DoMap $ Keyword a

-- | Parse any string into a token
noAbbrParser :: Parser Token
noAbbrParser = do
  a <- many1 alphaNum
  return $ NoToken a
