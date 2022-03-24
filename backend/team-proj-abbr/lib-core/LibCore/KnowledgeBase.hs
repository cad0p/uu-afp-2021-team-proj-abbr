module LibCore.KnowledgeBase where

import           Data.Map       (Map)
import           LibCore.Models (Keyword)

type KnowledgeBaseStructure = Map Keyword Keyword

getKnowledgeBase :: KnowledgeBaseStructure
getKnowledgeBase = undefined
