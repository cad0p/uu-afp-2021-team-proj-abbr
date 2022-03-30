module LibCore.Parser where

import           Data.Functor
import           LibCore.Models     (Keyword (Keyword), Token (DoMap, NoToken))
import           Text.Parsec
    ( ParseError
    , alphaNum
    , choice
    , many
    , many1
    , oneOf
    , parse
    , string
    )
import           Text.Parsec.String (Parser)

type ParseStructure
  = [Token]

-- | The abbreviation symbol. Can later be made configurable
abbSymbol :: String
abbSymbol = "@@"

-- | Given a string, parse it. This function can throw an error if parsing fails
doParse :: String -> ParseStructure
doParse s = case parseInput s of
  Left err -> error $ show err
  Right ps -> ps

-- | Map a string to a list of Tokens
parseInput :: String -> Either ParseError ParseStructure
parseInput = parse (mainParser abbSymbol) ""

-- | The main parser tries to consume all input into a ParseStructure, given an abbreviation symbol
mainParser :: String -> Parser ParseStructure
mainParser s = do many $ choice [lexeme $ abbrParser s, lexeme noAbbrParser]

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

-- | Consume whitespace characters
whitespace :: Parser ()
whitespace = void $ many $ oneOf [' ', '\n', '\t']

-- | Lexeme parsers consume the whitespace after them
lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace
