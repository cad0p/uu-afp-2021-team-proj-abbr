module LibCore.Parser where

import           LibCore.Models (Token (NoToken))

type ParseStructure
  = [Token]

-- | Map a string to a list of Tokens
parseInput :: String -> ParseStructure
parseInput s = map NoToken (words s)
