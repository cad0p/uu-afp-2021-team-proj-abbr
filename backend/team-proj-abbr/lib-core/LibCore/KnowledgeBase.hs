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

import           Control.Applicative  hiding (empty)
import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import qualified Data.Csv             as V
import           Data.Map             (Map, empty, fromList, singleton)
import qualified Data.Vector          as V
import           LibCore.Models       (Keyword (Keyword, keyword, plural))



type KnowledgeBaseStructure = Map Keyword Keyword


data KbEntry
  = KbEntry
      { abbreviation :: String
      , expansion    :: String
      }

instance FromNamedRecord KbEntry where
  parseNamedRecord r = KbEntry <$> r .: "abbreviation" <*> r .: "expansion"



mapEntries :: KbEntry -> (Keyword, Keyword)
mapEntries e =
  ( (Keyword { keyword = abbreviation e, plural = False })
  , (Keyword { keyword = expansion e, plural = False })
  )




{- 'getKnowledgeBase' parses from a CSV file to a 'KnowledgeBaseStructure'

-}
getKnowledgeBase
  :: V.Vector KbEntry -- ^ it is assumed to be a valid path to an existing KB with the format "*.csv"
  -> KnowledgeBaseStructure
getKnowledgeBase v = fromList (map mapEntries (V.toList v))
