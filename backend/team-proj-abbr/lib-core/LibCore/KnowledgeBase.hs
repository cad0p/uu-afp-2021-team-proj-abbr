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

buildKnowledgeBase :: [Keyword] -> KnowledgeBaseStructure
buildKnowledgeBase _ = undefined

list :: KnowledgeBaseStructure -> [Keyword]
list _ = []

get :: KnowledgeBaseStructure -> Keyword -> Either Error Keyword
get _ _ = Left "fail"

add :: KnowledgeBaseStructure -> Keyword -> Either Error Keyword
add _ _ = Left "fail"

update :: KnowledgeBaseStructure -> Keyword -> Either Error Keyword
update _ _ = Left "fail"

delete :: KnowledgeBaseStructure -> Keyword -> Either Error Bool
delete _ _ = Left "fail"


