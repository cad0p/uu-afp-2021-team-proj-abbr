{-|
Description : Defines the knowledge base, and a way to parse it from a file.
              For now, only '.csv' is supported
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

{-# LANGUAGE OverloadedStrings #-}


module LibCore.KnowledgeBase where

import           Data.Csv       (FromNamedRecord (parseNamedRecord), (.:))
import           Data.Map       (Map, fromList)
import qualified Data.Vector    as V
import           LibCore.Models (Keyword (Keyword, keyword, plural))



type KnowledgeBaseStructure = Map Keyword Keyword

-- | 'KbEntry' is an entry on the file containing the KB
data KbEntry
  = KbEntry
      { abbreviation :: String
      , expansion    :: String
      }

-- | 'FromNamedRecord' describes how to parse the record
--
-- Other examples of this can be found here:
-- https://github.com/haskell-hvr/cassava/blob/master/examples/
instance FromNamedRecord KbEntry where
  parseNamedRecord r = KbEntry <$> r .: "abbreviation" <*> r .: "expansion"


{-| 'mapEntries' maps entries to a pair of ('abbreviation', 'expansion')

>>> mapEntries (KbEntry { abbreviation = "abbr", expansion = "abbreviation" })
(Keyword {keyword = "abbr", plural = False},Keyword {keyword = "abbreviation", plural = False})

-}
mapEntries :: KbEntry -> (Keyword, Keyword)
mapEntries e =
  ( (Keyword { keyword = abbreviation e, plural = False })
  , (Keyword { keyword = expansion e, plural = False })
  )




{-| 'getKnowledgeBase' parses from a CSV file to a 'KnowledgeBaseStructure'

-}
getKnowledgeBase
  :: V.Vector KbEntry -- ^ it is the result of the low level parsing of the file, represented as a vector of entries
  -> KnowledgeBaseStructure
getKnowledgeBase v = fromList (map mapEntries (V.toList v))
