{-|
Description : TODO: defines the knowledge base
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCore.KnowledgeBase where

import           Data.Map       (Map, empty, toList)
import           LibCore.Models (Error (StandardError), Keyword (..))

type KnowledgeBaseStructure = Map Keyword Keyword

getKnowledgeBase :: KnowledgeBaseStructure
getKnowledgeBase = empty

buildKnowledgeBase :: [Keyword] -> KnowledgeBaseStructure
buildKnowledgeBase _ = undefined

-- |Get the list of all the stored records in the Knowledge Base.
--
-- Examples:
--
-- >>> testKB = Data.Map.fromList [(Keyword {keyword = "brb", plural = False}, Keyword {keyword = "be right back", plural = False})]
-- >>> listAll testKB
-- [(Keyword {keyword = "brb", plural = False},Keyword {keyword = "be right back", plural = False})]
listAll :: KnowledgeBaseStructure -> [(Keyword, Keyword)]
listAll = toList

get :: KnowledgeBaseStructure -> Keyword -> Either Error Keyword
get _ _ = Left $ StandardError "fail"

add :: KnowledgeBaseStructure -> Keyword -> Either Error Keyword
add _ _ = Left $ StandardError "fail"

update :: KnowledgeBaseStructure -> Keyword -> Either Error Keyword
update _ _ = Left $ StandardError "fail"

delete :: KnowledgeBaseStructure -> Keyword -> Either Error Bool
delete _ _ = Left $ StandardError "fail"


