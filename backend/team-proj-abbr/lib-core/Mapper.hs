module Mapper where

import           Data.Maybe    (fromMaybe)
import           KnowledgeBase (KnowledgeBaseStructure)
import           Parser        (ParseStructure, Token)

-- | Map a ParseStructure using a KnowledgeBaseStructure to another ParseStructure.
-- | The structure of this function makes it possible to apply multiple
-- | KnowledgeBases to one ParseStructure
mapParseStructure :: KnowledgeBaseStructure -> ParseStructure -> ParseStructure
mapParseStructure k = map (doLookup k)

-- | Lookup a token in the KnowledgeBaseStructure. If it is not found, return
-- | the lookup token. Otherwise, it returns the result of the lookup, which
-- | is also a token
doLookup :: KnowledgeBaseStructure -> Token -> Token
doLookup k t = fromMaybe t (lookup t k)
