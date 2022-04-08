{-|
Description : Decode a ParseStructure into a string
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCore.Decoder where

import           LibCore.Models (AKeyword (Key), Token (DoMap, NoToken))
import           LibCore.Parser (ParseStructure)

-- | The main entry point to convert a ParseStructure into a String
decode :: ParseStructure -> String
decode = concatMap tokenToString

-- | tokenToString maps each token class to a string representation
tokenToString :: Token -> String
tokenToString (NoToken s        ) = s
tokenToString (DoMap   (Key k _)) = k
