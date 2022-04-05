{-|
Description : TODO: defines the knowledge base
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCore.KnowledgeBase where

import           Data.Map       as M (Map, empty, fromList, lookup, toList)
import           LibCore.Models (Error (StandardError), Keyword (..))

type KnowledgeBaseStructure = M.Map Keyword Keyword

getKnowledgeBase :: KnowledgeBaseStructure
getKnowledgeBase = M.empty

buildKnowledgeBase :: [(Keyword, Keyword)] -> KnowledgeBaseStructure
buildKnowledgeBase = M.fromList

-- |Get the list of all the stored records in the Knowledge Base.
--
-- Examples:
--
-- >>> testKB = buildKnowledgeBase [(Keyword {keyword = "brb", plural = False}, Keyword {keyword = "be right back", plural = False})]
-- >>> listAll testKB
-- [(Keyword {keyword = "brb", plural = False},Keyword {keyword = "be right back", plural = False})]
listAll :: KnowledgeBaseStructure -> [(Keyword, Keyword)]
listAll = M.toList

-- |Retrieve a single element by its keyword.
--
-- Examples
--
-- >>> testKB = buildKnowledgeBase [(Keyword {keyword = "brb", plural = False}, Keyword {keyword = "be right back", plural = False})]
-- >>> get testKB (Keyword {keyword = "brb", plural = False})
-- Right (Keyword {keyword = "be right back", plural = False})
--
-- >>> get testKB (Keyword {keyword = "beb", plural = False})
-- Left (StandardError "no record found for this keyword")
get :: KnowledgeBaseStructure -> Keyword -> Either Error Keyword
get kb k = case M.lookup k kb of
  Nothing -> Left $ StandardError "no record found for this keyword"
  Just x  -> Right x

add :: KnowledgeBaseStructure -> Keyword -> Either Error Keyword
add _ _ = Left $ StandardError "fail"

update :: KnowledgeBaseStructure -> Keyword -> Either Error Keyword
update _ _ = Left $ StandardError "fail"

delete :: KnowledgeBaseStructure -> Keyword -> Either Error Bool
delete _ _ = Left $ StandardError "fail"


