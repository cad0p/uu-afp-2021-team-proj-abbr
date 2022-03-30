{-# LANGUAGE DeriveDataTypeable #-}

{-|
Module      : LibCli.CliSpec
Description : Command Line Interface - Specification
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCli.CliSpec
    ( cliModes
    , ShortHndr(..)
    ) where

import qualified System.Console.CmdArgs as CMD

-----------------------------------
-- CLI interface specificaitons: --
-----------------------------------

-- |ShortHndr CLI interface specification.
data ShortHndr
  -- |Defines the arguments for the replace command
  = Replace
      { input   :: Maybe FilePath
      , out     :: Maybe FilePath
      , inplace :: Maybe Bool
      , kb      :: Maybe FilePath
      }
  -- |Defines the arguments for the expand command
  | Expand
      { abbreviation :: String
      , kb           :: Maybe FilePath
      }
  -- |Defines the arguments for the list command
  | List
      { kb :: Maybe FilePath
      }
  -- |Defines the arguments for the add command
  | Add
      { abbreviation :: String
      , expansion    :: String
      , kb           :: Maybe FilePath
      }
  -- |Defines the arguments for the update command
  | Update
      { abbreviation :: String
      , expansion    :: String
      , kb           :: Maybe FilePath
      }
  -- |Defines the arguments for the delete command
  | Delete
      { abbreviation :: String
      , kb           :: Maybe FilePath
      }
  deriving (CMD.Data, CMD.Typeable, Show)

-- |Utility function to provide help for the file type arguments.
fileFlags :: String -> Maybe FilePath -> Maybe FilePath
fileFlags h f = f CMD.&= CMD.help h CMD.&= CMD.typFile

-------------------------
-- Expansion commands: --
-------------------------

replace :: ShortHndr
replace =
    Replace
            { input   = fileFlags "Source file" (pure "shorthndr-input.txt")
            , out     = fileFlags "Output file" (pure "shorthndr--out.txt")
            , kb      = fileFlags "Knowledge Base source file"
                                  (pure "shorthndr-kb.csv")
            , inplace = CMD.def
            }
        CMD.&= CMD.help
                   "Replace all abreviations in the provided file with their expansions"

expand :: ShortHndr
expand =
    Expand
            { abbreviation = CMD.def
            , kb           = fileFlags "Knowledge Base source file"
                                       (pure "shorthndr-kb.csv")
            }
        CMD.&= CMD.help
                   "Expand a provided abbreviation abbreviation if one is found"

expansionModes :: [ShortHndr]
expansionModes = [replace, expand]

------------------------------
-- Knowledge Base commands: --
------------------------------

list :: ShortHndr
list =
    List { kb = fileFlags "Knowledge Base source file" (pure "shorthndr-kb.csv")
         }
        CMD.&= CMD.help "List all records of the Knowledge Base"

add :: ShortHndr
add =
    Add { abbreviation = CMD.def
        , expansion = CMD.def
        , kb = fileFlags "Knowledge Base source file" (pure "shorthndr-kb.csv")
        }
        CMD.&= CMD.help "Add a new abbreviation record to the Knowledge Base"

update :: ShortHndr
update =
    Update
            { abbreviation = CMD.def
            , expansion    = CMD.def
            , kb           = fileFlags "Knowledge Base source file"
                                       (pure "shorthndr-kb.csv")
            }
        CMD.&= CMD.help
                   "Update an existing abbreviation record in the Knowledge Base"

delete :: ShortHndr
delete =
    Delete
            { abbreviation = CMD.def
            , kb           = fileFlags "Knowledge Base source file"
                                       (pure "shorthndr-kb.csv")
            }
        CMD.&= CMD.help "Delete an abbreviation record from the Knowledge Base"

kbModes :: [ShortHndr]
kbModes = [list, add, update, delete]

-----------------------------
-- All exported CLI modes: --
-----------------------------

cliModes :: [ShortHndr]
cliModes = expansionModes ++ kbModes
