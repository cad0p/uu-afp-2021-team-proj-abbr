{-|
Description : Defines the knowledge base, and a way to parse it from a file.
              For now, only '.csv' is supported
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCore.KnowledgeBase where

import           Data.Map       (Map, empty)
import           LibCore.Models (Keyword)


type KnowledgeBaseStructure = Map Keyword Keyword

getKnowledgeBase :: FilePath -> KnowledgeBaseStructure
getKnowledgeBase _ = empty
