{-|
Description : TODO: Parses stuff
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCore.Parser where

import           LibCore.Models (Token (NoToken))

type ParseStructure = [Token]

-- | Map a string to a list of Tokens
--
-- Examples:
--
-- >>> parseInput "Hello world, this is a test"
-- [NoToken "Hello",NoToken "world,",NoToken "this",NoToken "is",NoToken "a",NoToken "test"]
parseInput :: String -> ParseStructure
parseInput s = map NoToken (words s)
