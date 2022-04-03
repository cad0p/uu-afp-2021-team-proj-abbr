{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}

{-|
Description : Command Line Interface - Specification
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCli.Spec where

import qualified System.Console.CmdArgs          as CMD
import           System.Console.CmdArgs.Explicit
    ( Mode
    , flagArg
    , flagHelpSimple
    , flagNone
    , flagReq
    , mode
    , modes
    )

-----------------------------------
-- CLI interface specificaitons: --
-----------------------------------

data Expansion where
  Re :: Replace -> Expansion
  Ex :: Expand -> Expansion
  deriving (CMD.Data, CMD.Typeable, Show)

data KnowledgeBaseTypes where
  Lst :: List -> KnowledgeBaseTypes
  Ad :: Add -> KnowledgeBaseTypes
  Up :: Update -> KnowledgeBaseTypes
  Del :: Delete -> KnowledgeBaseTypes
  deriving (CMD.Data, CMD.Typeable, Show)

data ShortHndrModes where
  Exp :: Expansion -> ShortHndrModes
  Kbt :: KnowledgeBaseTypes -> ShortHndrModes
  Hlp :: ShortHndrModes
  deriving (CMD.Data, CMD.Typeable, Show)


-- |ShortHndr CLI interface specification.
-- |Defines the arguments for the replace command
data Replace
  = Replace
      { input      :: Maybe FilePath
      , out        :: Maybe FilePath
      , inplace    :: Maybe Bool
      , replace_kb :: Maybe FilePath
      }
  deriving (CMD.Data, CMD.Typeable, Show)

-- |Defines the arguments for the expand command
data Expand
  = Expand
      { expand_abbr :: String
      , expand_kb   :: Maybe FilePath
      }
  deriving (CMD.Data, CMD.Typeable, Show)

-- |Defines the arguments for the list command
newtype List
  = List { list_kb :: Maybe FilePath }
  deriving (CMD.Data, CMD.Typeable, Show)

-- |Defines the arguments for the add command
data Add
  = Add
      { add_abbr      :: String
      , add_expansion :: String
      , add_kb        :: Maybe FilePath
      }
  deriving (CMD.Data, CMD.Typeable, Show)

-- |Defines the arguments for the update command
data Update
  = Update
      { update_abbr      :: String
      , update_expansion :: String
      , update_kb        :: Maybe FilePath
      }
  deriving (CMD.Data, CMD.Typeable, Show)

-- |Defines the arguments for the delete command
data Delete
  = Delete
      { delete_abbr :: String
      , delete_kb   :: Maybe FilePath
      }
  deriving (CMD.Data, CMD.Typeable, Show)

-- |Utility function to provide help for the file type arguments.
fileFlags :: String -> Maybe FilePath -> Maybe FilePath
fileFlags h f = f CMD.&= CMD.help h CMD.&= CMD.typFile

-------------------------
-- Expansion commands: --
-------------------------

replace :: ShortHndrModes
replace = Exp ( Re $
    Replace
            { input   = fileFlags "Source file" (pure "shorthndr-input.txt")
            , out     = fileFlags "Output file" (pure "shorthndr--out.txt")
            , replace_kb      = fileFlags "Knowledge Base source file"
                                  (pure "shorthndr-kb.csv")
            , inplace = CMD.def
            })
        CMD.&= CMD.help
                   "Replace all abreviations in the provided file with their expansions"

expand :: ShortHndrModes
expand = Exp ( Ex $
    Expand
            { expand_abbr = CMD.def
            , expand_kb           = fileFlags "Knowledge Base source file"
                                       (pure "shorthndr-kb.csv")
            })
        CMD.&= CMD.help
                   "Expand a provided abbreviation abbreviation if one is found"


------------------------------
-- Knowledge Base commands: --
------------------------------

list :: ShortHndrModes
list = Kbt ( Lst $
    List { list_kb = fileFlags "Knowledge Base source file" (pure "shorthndr-kb.csv")
         })
        CMD.&= CMD.help "List all records of the Knowledge Base"

add :: ShortHndrModes
add = Kbt ( Ad $
    Add { add_abbr = CMD.def
        , add_expansion = CMD.def
        , add_kb = fileFlags "Knowledge Base source file" (pure "shorthndr-kb.csv")
        })
        CMD.&= CMD.help "Add a new abbreviation record to the Knowledge Base"

update :: ShortHndrModes
update = Kbt ( Up $
    Update
            { update_abbr = CMD.def
            , update_expansion    = CMD.def
            , update_kb           = fileFlags "Knowledge Base source file"
                                       (pure "shorthndr-kb.csv")
            })
        CMD.&= CMD.help
                   "Update an existing abbreviation record in the Knowledge Base"

delete :: ShortHndrModes
delete = Kbt (Del $
    Delete
            { delete_abbr = CMD.def
            , delete_kb           = fileFlags "Knowledge Base source file"
                                       (pure "shorthndr-kb.csv")
            })
        CMD.&= CMD.help "Delete an abbreviation record from the Knowledge Base"


-----------------------------
-- All exported CLI modes: --
-----------------------------

cliModes :: [ShortHndrModes]
cliModes = [replace, expand, list, add, update, delete]

defaultMode :: ShortHndrModes
defaultMode = Hlp

helpMode :: ShortHndrModes -> ShortHndrModes
helpMode _ = Hlp

-- Explicit arguments
arguments :: Mode ShortHndrModes
arguments = modes "ShortHandr" defaultMode "Use 'replace' to enter replace mode" [replaceArgs]

replaceArgs :: Mode ShortHndrModes
replaceArgs = mode "replace" initial
    "Replace all abreviations in the provided file with their expansions"
    (flagArg (updateMode "") "yes")
    [
    flagReq ["input", "i"] (updateMode "input") "FILENAME" "Input filename"
    ,flagReq ["out", "o"] (updateMode "out") "FILENAME" "Output filename"
    ,flagReq ["k"] (updateMode "replace_kb") "FILENAME" "Knowledgebase filename"
    ,flagNone ["inplace"] (setInplace True) "inplace"
    ,flagHelpSimple helpMode
    ]
    where initial = Exp $ Re $ Replace { input = Just "", out = Just "", replace_kb = Just "", inplace = Just False }

setInplace :: Bool -> ShortHndrModes -> ShortHndrModes
setInplace b (Exp (Re r)) = Exp $ Re $ r {inplace = Just b}
setInplace _ r            = r

updateMode :: String -> String -> ShortHndrModes -> Either String ShortHndrModes
updateMode "input" s (Exp (Re r)) = Right $ Exp $ Re $ r {input = Just s}
updateMode "out" s (Exp (Re r)) = Right $ Exp $ Re $ r {out = Just s}
updateMode "replace_kb" s (Exp (Re r)) = Right $ Exp $ Re $ r {replace_kb = Just s}
updateMode _ _ e@(Exp (Re _))       = Right e
updateMode _ _ (Exp (Ex _))         = undefined
updateMode _ _ (Kbt _)              = undefined
updateMode _ _ Hlp              = undefined
