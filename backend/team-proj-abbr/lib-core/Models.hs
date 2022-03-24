module Models where

-- | We parse a string into a NoToken if it does not match the syntax for
-- | parsing. The DoMap is a string that has to be mapped. In our proposals,
-- | the DoMap strings start with @@
data Token
  = NoToken String
  | DoMap String
  deriving (Eq, Ord, Show)
