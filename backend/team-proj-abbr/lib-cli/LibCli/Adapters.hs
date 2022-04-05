{-|
Description : Support for various IO adapters to the LibCli entities.
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

{-# LANGUAGE OverloadedStrings #-}

module LibCli.Adapters where

import           Data.Csv
    ( FromNamedRecord (parseNamedRecord)
    , (.:)
    )
import qualified Data.Vector           as V
import qualified LibCore.KnowledgeBase as KB (KnowledgeBaseStructure, build)
import           LibCore.Models        (Keyword (..))


--------------------------------
-- Knowledge Base CSV loading --
--------------------------------

-- |'KbEntry' is an entry on the file containing the Knowledge Base.
data KbEntry
  = KbEntry
      { abbreviation :: String
      , expansion    :: String
      }

-- | 'FromNamedRecord' describes how to parse the record
instance FromNamedRecord KbEntry where
  parseNamedRecord r = KbEntry <$> r .: "abbreviation" <*> r .: "expansion"


-- |'mapEntries' maps entries to a pair of ('abbreviation', 'expansion')
mapEntries :: KbEntry -> (Keyword, Keyword)
mapEntries e =
  ( (Keyword { keyword = abbreviation e, plural = False })
  , (Keyword { keyword = expansion e, plural = False })
  )

-- |'getKnowledgeBase' parses from a CSV file to a 'KnowledgeBaseStructure'.
getKnowledgeBase
  :: V.Vector KbEntry -- ^ it is the result of the low level parsing of the file, represented as a vector of entries
  -> KB.KnowledgeBaseStructure
getKnowledgeBase v = KB.build $ map mapEntries $ V.toList v
