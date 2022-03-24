module LibCore.KnowledgeBase where

import           Data.Map       (Map)
import           LibCore.Models (Token)

type KnowledgeBaseStructure = Map Token Token

getKnowledgeBase :: KnowledgeBaseStructure
getKnowledgeBase = undefined
