module Parser where

import           Models (Token (NoToken))

type ParseStructure
  = [Token]

-- | Map a string to a list of Tokens
parseInput :: String -> ParseStructure
parseInput s = map NoToken (words s)
