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
import           Data.Map             (Map, empty)
import qualified Data.Vector          as V
import           LibCore.Models       (Keyword (Keyword, keyword, plural))



type KnowledgeBaseStructure = Map Keyword Keyword


data KbCsvEntry
  = KbCsvEntry
      { abbreviation :: String
      , expansion    :: String
      }

instance FromNamedRecord KbCsvEntry where
  parseNamedRecord r = KbCsvEntry <$> r .: "abbreviation" <*> r .: "expansion"

{- 'getKnowledgeBase' parses from a CSV file to a 'KnowledgeBaseStructure'

-}
getKnowledgeBase
  :: FilePath -- ^ it is assumed to be a valid path to an existing KB with the format "*.csv"
  -> KnowledgeBaseStructure
getKnowledgeBase fp = do
  kbs     <- (empty :: Map Keyword Keyword)
  csvData <- BL.readFile fp
  case decodeByName csvData of
    Left  err    -> error ("decoding" ++ err)
    Right (_, v) -> V.forM_
      v
      (\e -> do-- e stands for kb entry
        insert (Keyword { keyword = abbreviation e, plural = False })
               (Keyword { keyword = expansion e, plural = False })
               kbs
        kbs
      )
