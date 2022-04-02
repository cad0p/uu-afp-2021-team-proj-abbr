{-|
Description : TODO: defines the knowledge base
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCore.KnowledgeBase where

import           Data.Map       (Map, empty)
import           LibCore.Models (Keyword)

type KnowledgeBaseStructure = Map Keyword Keyword

getKnowledgeBase :: KnowledgeBaseStructure
getKnowledgeBase = empty
