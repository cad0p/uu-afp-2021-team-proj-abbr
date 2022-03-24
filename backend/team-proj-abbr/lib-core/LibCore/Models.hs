module LibCore.Models where

-- | We parse a string into a NoToken if it does not match the syntax for
-- | parsing. The DoMap is a string that has to be mapped. In our proposals,
-- | the DoMap strings start with @@
data Token
  = NoToken String
  | DoMap Keyword
  deriving (Eq, Ord, Show)

newtype Keyword
  = Keyword String
  deriving (Eq, Ord, Show)
