{-|
Description : Defines the knowledge base implementation and abstractions.
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCore.KnowledgeBase where

import           Data.Map       as M
    ( Map
    , delete
    , fromList
    , insert
    , lookup
    , toList
    , update
    )
import           LibCore.Models (Error (StandardError), Keyword)

-- TODO(future ideas): support partial updates and state? RWS monad?
type KnowledgeBaseStructure = M.Map Keyword Keyword


-- | Build a new Knowledge Base from the supplied keyword pair list.
build :: [(Keyword, Keyword)] -> KnowledgeBaseStructure
build = M.fromList

-- TODO(tech-debt): make real tests.

-- |Get the list of all the stored records in the Knowledge Base.
--
-- Examples:
--
-- >>> testKB = build [(pure "brb", pure "be right back")]
-- >>> listAll testKB
-- [(Key {keyword = "brb", plural = False},Key {keyword = "be right back", plural = False})]
--
listAll :: KnowledgeBaseStructure -> [(Keyword, Keyword)]
listAll = M.toList


-- |Retrieve a single element by its keyword.
--
-- Examples
--
-- >>> testKB = build [(pure "brb", pure "be right back")]
-- >>> get testKB $ pure "brb"
-- Right (Key {keyword = "be right back", plural = False})
--
-- >>> get testKB $ pure "beb"
-- Left (StandardError "no record found for this keyword : Key {keyword = \"beb\", plural = False}")
--
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
-- >>> testKB = build [(pure "brb", pure "be right back")]
-- >>> add testKB (pure "btw") (pure "by the way")
-- (Key {keyword = "by the way", plural = False},fromList [(Key {keyword = "brb", plural = False},Key {keyword = "be right back", plural = False}),(Key {keyword = "btw", plural = False},Key {keyword = "by the way", plural = False})])
--
-- >>> add testKB (pure "btw") (pure "by the way!")
-- (Key {keyword = "by the way!", plural = False},fromList [(Key {keyword = "brb", plural = False},Key {keyword = "be right back", plural = False}),(Key {keyword = "btw", plural = False},Key {keyword = "by the way!", plural = False})])
--
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
-- >>> testKB = build [(pure "brb", pure "be right back")]
-- >>> put testKB (pure "brb") (pure "BE RIGHT BACK")
-- Right (Key {keyword = "BE RIGHT BACK", plural = False},fromList [(Key {keyword = "brb", plural = False},Key {keyword = "BE RIGHT BACK", plural = False})])
--
-- >>> put testKB (pure "btw") (pure "by the way!")
-- Left (StandardError "no record found for this keyword : Key {keyword = \"btw\", plural = False}")
--
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
-- >>> testKB = build [(pure "brb", pure "be right back")]
-- >>> remove testKB $ pure "brb"
-- Right (fromList [])
--
-- >>> remove testKB $ pure "btw"
-- Left (StandardError "no record found for this keyword : Key {keyword = \"btw\", plural = False}")
--
remove
  :: KnowledgeBaseStructure -> Keyword -> Either Error KnowledgeBaseStructure
remove kb k = case get kb k of
  Left  er -> Left er
  Right _  -> Right $ M.delete k kb

