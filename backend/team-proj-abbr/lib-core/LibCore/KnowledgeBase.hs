{-|
Description : The knowledge base implementation.
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCore.KnowledgeBase where

import           Data.Map       as M
    ( Map
    , delete
    , empty
    , fromList
    , insert
    , lookup
    , toList
    , update
    )
import           LibCore.Models (Error (StandardError), Keyword (..))

-- TODO(future focus): support partial updates and state? RWS monad?
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
-- Left (StandardError "no record found for this keyword : Keyword {keyword = \"beb\", plural = False}")
get :: KnowledgeBaseStructure -> Keyword -> Either Error Keyword
get kb k = case M.lookup k kb of
  Nothing ->
    Left $ StandardError $ "no record found for this keyword : " ++ show k
  Just x -> Right x


-- |Add new item to the Knowledge Base.
-- Adds the item to the KB as a new record if the key was notseen before,
-- and replaces the existing record otherwise.
--
-- Examples
--
-- >>> testKB = buildKnowledgeBase [(Keyword {keyword = "brb", plural = False}, Keyword {keyword = "be right back", plural = False})]
-- >>> add testKB (Keyword {keyword = "btw", plural = False}) (Keyword {keyword = "by the way", plural = False})
-- (Keyword {keyword = "by the way", plural = False},fromList [(Keyword {keyword = "brb", plural = False},Keyword {keyword = "be right back", plural = False}),(Keyword {keyword = "btw", plural = False},Keyword {keyword = "by the way", plural = False})])
--
-- >>> add testKB (Keyword {keyword = "btw", plural = False}) (Keyword {keyword = "by the way!", plural = False})
-- (Keyword {keyword = "by the way!", plural = False},fromList [(Keyword {keyword = "brb", plural = False},Keyword {keyword = "be right back", plural = False}),(Keyword {keyword = "btw", plural = False},Keyword {keyword = "by the way!", plural = False})])
add
  :: KnowledgeBaseStructure
  -> Keyword
  -> Keyword
  -> (Keyword, KnowledgeBaseStructure)
add kb k r = (r, M.insert k r kb)


-- |Updates the record at given key with the new value.
-- Only full updates are supported.
-- If the key is not in the KB, returns an error.
--
-- Examples:
--
-- >>> testKB = buildKnowledgeBase [(Keyword {keyword = "brb", plural = False}, Keyword {keyword = "be right back", plural = False})]
-- >>> put testKB (Keyword {keyword = "brb", plural = False}) (Keyword {keyword = "BE RIGHT BACK", plural = False})
-- Right (Keyword {keyword = "BE RIGHT BACK", plural = False},fromList [(Keyword {keyword = "brb", plural = False},Keyword {keyword = "BE RIGHT BACK", plural = False})])
--
-- >>> put testKB (Keyword {keyword = "btw", plural = False}) (Keyword {keyword = "by the way!", plural = False})
-- Left (StandardError "no record found for this keyword : Keyword {keyword = \"btw\", plural = False}")
put
  :: KnowledgeBaseStructure
  -> Keyword
  -> Keyword
  -> Either Error (Keyword, KnowledgeBaseStructure)
put kb k r = case get kb k of
  Left  er -> Left er
  Right _  -> Right (r, M.update (\_ -> Just r) k kb)


-- |Removes an existing item at the given key form the Knowledge Base.
-- If the key is not present, return as error.
--
-- Examples:
--
-- >>> testKB = buildKnowledgeBase [(Keyword {keyword = "brb", plural = False}, Keyword {keyword = "be right back", plural = False})]
-- >>> remove testKB (Keyword {keyword = "brb", plural = False})
-- Right (fromList [])
--
-- >>> remove testKB (Keyword {keyword = "btw", plural = False})
-- Left (StandardError "no record found for this keyword : Keyword {keyword = \"btw\", plural = False}")
remove
  :: KnowledgeBaseStructure -> Keyword -> Either Error KnowledgeBaseStructure
remove kb k = case get kb k of
  Left  er -> Left er
  Right _  -> Right $ M.delete k kb


