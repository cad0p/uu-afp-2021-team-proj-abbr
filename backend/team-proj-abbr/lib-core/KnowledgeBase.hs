module KnowledgeBase where

import           Data.Map (Map)
import           Models   (Token)

type KnowledgeBaseStructure = Map Token Token

getKnowledgeBase :: KnowledgeBaseStructure
getKnowledgeBase = undefined
