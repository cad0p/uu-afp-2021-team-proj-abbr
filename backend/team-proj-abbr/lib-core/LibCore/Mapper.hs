{-|
Description : TODO: Maps stuff
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCore.Mapper where

import qualified Data.Map              as M
import           LibCore.KnowledgeBase (KnowledgeBaseStructure)
import           LibCore.Models        (Token (DoMap, NoToken))
import           LibCore.Parser        (ParseStructure)

-- | Map a ParseStructure using a KnowledgeBaseStructure to another ParseStructure.
-- The structure of this function makes it possible to apply multiple
-- KnowledgeBases to one ParseStructure
mapParseStructure :: KnowledgeBaseStructure -> ParseStructure -> ParseStructure
mapParseStructure k = map (doLookup k)

-- | Lookup a token in the KnowledgeBaseStructure. If it is not found, return
-- the lookup token. Otherwise, it returns the result of the lookup, which
-- is also a token
doLookup :: KnowledgeBaseStructure -> Token -> Token
doLookup _ n@(NoToken _ ) = n
doLookup k n@(DoMap   kw) = maybe n DoMap (M.lookup kw k)
