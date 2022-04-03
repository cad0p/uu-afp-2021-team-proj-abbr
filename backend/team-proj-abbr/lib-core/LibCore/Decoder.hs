{-|
Description : TODO: Decodes stuff
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCore.Decoder where

import           LibCore.Models (Keyword (Keyword), Token (DoMap, NoToken))
import           LibCore.Parser (ParseStructure)

decode :: ParseStructure -> String
decode s = unwords $ map tokenToString s

tokenToString :: Token -> String
tokenToString (NoToken s)           = s
tokenToString (DoMap (Keyword k _)) = k
