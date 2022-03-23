module Parser where

newtype Token
  = Token String
  deriving (Eq, Ord, Show)

type ParseStructure
  = [Token]

-- | Map a string to a list of Tokens
parseInput :: String -> ParseStructure
parseInput s = map Token (words s)
