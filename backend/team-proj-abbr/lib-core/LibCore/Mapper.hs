module LibCore.Mapper where

import qualified Data.Map              as M
import           Data.Maybe            (fromMaybe)
import           LibCore.KnowledgeBase (KnowledgeBaseStructure)
import           LibCore.Models        (Token)
import           LibCore.Parser        (ParseStructure)

-- | Map a ParseStructure using a KnowledgeBaseStructure to another ParseStructure.
-- | The structure of this function makes it possible to apply multiple
-- | KnowledgeBases to one ParseStructure
mapParseStructure :: KnowledgeBaseStructure -> ParseStructure -> ParseStructure
mapParseStructure k = map (doLookup k)

-- | Lookup a token in the KnowledgeBaseStructure. If it is not found, return
-- | the lookup token. Otherwise, it returns the result of the lookup, which
-- | is also a token
doLookup :: KnowledgeBaseStructure -> Token -> Token
doLookup k t = fromMaybe t (M.lookup t k)


