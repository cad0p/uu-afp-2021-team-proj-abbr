{-|
Description : TODO: Models stuff
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCore.Models where

-- | We parse a string into a NoToken if it does not match the syntax for
-- | parsing. The DoMap is a string that has to be mapped. In our proposals,
-- | the DoMap strings start with @@
data Token
  = NoToken String
  | DoMap Keyword
  deriving (Eq, Ord, Show)

data Keyword
  = Keyword
      { keyword :: String
      , plural  :: Bool
      }
  deriving (Eq, Ord, Show)

-- |Standard representation of the abbreviation record.
data AbbreviationRecord
  = AbbreviationRecord
      { keyword   :: Keyword
      , expansion :: Keyword
      }
