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
    ( DefaultOrdered (headerOrder)
    , FromNamedRecord (parseNamedRecord)
    , ToNamedRecord (toNamedRecord)
    , header
    , namedRecord
    , (.:)
    , (.=)
    )
import qualified Data.Vector           as V
import qualified LibCore.KnowledgeBase as KB (KnowledgeBaseStructure, build)
import           LibCore.Models        (AKeyword (..), Keyword)


--------------------------------
-- Knowledge Base CSV loading --
--------------------------------

-- |'KbEntry' is an entry on the file containing the Knowledge Base.
data KbEntry
  = KbEntry
      { abbreviation :: String
      , expansion    :: String
      }

-- TODO(tech debt): make use of the Generics as in this example:
--    https://hackage.haskell.org/package/cassava-0.5.2.0/docs/Data-Csv.html#g:4
-- | 'FromNamedRecord' describes how to parse the record
--
-- Other examples of this can be found here:
-- https://github.com/haskell-hvr/cassava/blob/master/examples/
instance FromNamedRecord KbEntry where
  parseNamedRecord r = KbEntry <$> r .: "abbreviation" <*> r .: "expansion"

instance ToNamedRecord KbEntry where
  toNamedRecord KbEntry { abbreviation = a, expansion = e } =
    namedRecord ["abbreviation" .= a, "expansion" .= e]

instance DefaultOrdered KbEntry where
  headerOrder _ = header ["abbreviation", "expansion"]


{-| 'mapEntries' maps entries to a pair of ('abbreviation', 'expansion')

>>> mapEntries (KbEntry { abbreviation = "abbr", expansion = "abbreviation" })
(Keyword {keyword = "abbr", plural = False},Keyword {keyword = "abbreviation", plural = False})

-}
mapEntries :: KbEntry -> (Keyword, Keyword)
mapEntries e =
  ( (Keyword { keyword = abbreviation e, plural = False })
  , (Keyword { keyword = expansion e, plural = False })
  )

-- |`mapKeywords` maps keyword pairs to their entries
mapKeywordPair :: (Keyword, Keyword) -> KbEntry
mapKeywordPair (Keyword kk _, Keyword vk _) = KbEntry kk vk

-- |'getKnowledgeBase' parses from a CSV file to a 'KnowledgeBaseStructure'.
getKnowledgeBase
  :: V.Vector KbEntry -- ^ it is the result of the low level parsing of the file, represented as a vector of entries
  -> KB.KnowledgeBaseStructure
getKnowledgeBase v = KB.build $ map mapEntries $ V.toList v

