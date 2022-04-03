{-# LANGUAGE GADTs #-}

{-|
Description : Command Line Interface - Specification
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCli.Spec where

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
  deriving (Show)

data KnowledgeBaseTypes where
  Lst :: List -> KnowledgeBaseTypes
  Ad :: Add -> KnowledgeBaseTypes
  Up :: Update -> KnowledgeBaseTypes
  Del :: Delete -> KnowledgeBaseTypes
  deriving (Show)

data ShortHndrModes where
  Exp :: Expansion -> ShortHndrModes
  Kbt :: KnowledgeBaseTypes -> ShortHndrModes
  Hlp :: ShortHndrModes
  deriving (Show)


-- |ShortHndr CLI interface specification.
-- |Defines the arguments for the replace command
data Replace
  = Replace
      { input      :: Maybe FilePath
      , out        :: Maybe FilePath
      , inplace    :: Maybe Bool
      , replace_kb :: Maybe FilePath
      }
  deriving (Show)

-- |Defines the arguments for the expand command
data Expand
  = Expand
      { expand_abbr :: String
      , expand_kb   :: Maybe FilePath
      }
  deriving (Show)

-- |Defines the arguments for the list command
newtype List
  = List { list_kb :: Maybe FilePath }
  deriving (Show)

-- |Defines the arguments for the add command
data Add
  = Add
      { add_abbr      :: String
      , add_expansion :: String
      , add_kb        :: Maybe FilePath
      }
  deriving (Show)

-- |Defines the arguments for the update command
data Update
  = Update
      { update_abbr      :: String
      , update_expansion :: String
      , update_kb        :: Maybe FilePath
      }
  deriving (Show)

-- |Defines the arguments for the delete command
data Delete
  = Delete
      { delete_abbr :: String
      , delete_kb   :: Maybe FilePath
      }
  deriving (Show)


-----------------------------
-- All exported CLI modes: --
-----------------------------

defaultMode :: ShortHndrModes
defaultMode = Hlp

helpMode :: ShortHndrModes -> ShortHndrModes
helpMode _ = Hlp

-- Explicit arguments
arguments :: Mode ShortHndrModes
arguments = modes "ShortHandr" defaultMode "Use 'replace' to enter replace mode" [replaceArgs, expandArgs, listArgs, addArgs, updateArgs, deleteArgs]

replaceArgs :: Mode ShortHndrModes
replaceArgs = mode "replace" initial
    "Replace all abreviations in the provided file with their expansions"
    (flagArg (updateMode "") "replace")
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

expandArgs :: Mode ShortHndrModes
expandArgs = mode "expand" initial
    "Expand a provided abbreviation abbreviation if one is found"
    (flagArg (updateMode "abbr") "abbreviation")
    [
    flagReq ["k"] (updateMode "expand_kb") "FILENAME" "Knowledgebase filename"
    ,flagHelpSimple helpMode
    ]
    where initial = Exp $ Ex $ Expand { expand_abbr = "", expand_kb = Just "" }

listArgs :: Mode ShortHndrModes
listArgs = mode "list" initial
    "List all records of the Knowledge Base"
    (flagArg (updateMode "") "list")
    [
    flagReq ["k"] (updateMode "list_kb") "FILENAME" "Knowledgebase filename"
    ,flagHelpSimple helpMode
    ]
    where initial = Kbt $ Lst $ List { list_kb = Just "" }

addArgs :: Mode ShortHndrModes
addArgs = mode "add" initial
    "Add a new abbreviation record to the Knowledge Base"
    (flagArg (updateMode "") "add")
    [
    flagReq ["k"] (updateMode "add_kb") "FILENAME" "Knowledgebase filename"
    ,flagReq ["ex"] (updateMode "add_ex") "expansion" "Abbreviation expansion"
    ,flagReq ["abbr"] (updateMode "add_abbr") "abbreviation" "Abbreviation"
    ,flagHelpSimple helpMode
    ]
    where initial = Kbt $ Ad $ Add { add_abbr = "", add_expansion = "", add_kb = Just "" }

updateArgs :: Mode ShortHndrModes
updateArgs = mode "update" initial
    "Update an existing abbreviation record in the Knowledge Base"
    (flagArg (updateMode "") "update")
    [
    flagReq ["k"] (updateMode "update_kb") "FILENAME" "Knowledgebase filename"
    ,flagReq ["ex"] (updateMode "update_ex") "expansion" "Abbreviation expansion"
    ,flagReq ["abbr"] (updateMode "update_abbr") "abbreviation" "Abbreviation"
    ,flagHelpSimple helpMode
    ]
    where initial = Kbt $ Up $ Update { update_abbr = "", update_expansion = "", update_kb = Just "" }

deleteArgs :: Mode ShortHndrModes
deleteArgs = mode "delete" initial
    "Delete an abbreviation record from the Knowledge Base"
    (flagArg (updateMode "") "delete")
    [
    flagReq ["k"] (updateMode "delete_kb") "FILENAME" "Knowledgebase filename"
    ,flagReq ["abbr"] (updateMode "delete_abbr") "abbreviation" "Abbreviation"
    ,flagHelpSimple helpMode
    ]
    where initial = Kbt $ Del $ Delete { delete_abbr = "", delete_kb = Just "" }

updateMode :: String -> String -> ShortHndrModes -> Either String ShortHndrModes
updateMode "input" s (Exp (Re r)) = Right $ Exp $ Re $ r {input = Just s}
updateMode "out" s (Exp (Re r)) = Right $ Exp $ Re $ r {out = Just s}
updateMode "replace_kb" s (Exp (Re r)) = Right $ Exp $ Re $ r {replace_kb = Just s}
updateMode "expand_kb" s (Exp (Ex r))         = Right $ Exp $ Ex $ r { expand_kb = Just s}
updateMode "abbr" s (Exp (Ex r))         = Right $ Exp $ Ex $ r { expand_abbr = s }
updateMode "list_kb" s (Kbt (Lst r))         = Right $ Kbt $ Lst $ r { list_kb = Just s}
updateMode "add_kb" s (Kbt (Ad r))         = Right $ Kbt $ Ad $ r { add_kb = Just s}
updateMode "add_ex" s (Kbt (Ad r))         = Right $ Kbt $ Ad $ r { add_expansion = s}
updateMode "add_abbr" s (Kbt (Ad r))         = Right $ Kbt $ Ad $ r { add_abbr = s}
updateMode "update_kb" s (Kbt (Up r))         = Right $ Kbt $ Up $ r { update_kb = Just s}
updateMode "update_ex" s (Kbt (Up r))         = Right $ Kbt $ Up $ r { update_expansion = s}
updateMode "update_abbr" s (Kbt (Up r))         = Right $ Kbt $ Up $ r { update_abbr = s}
updateMode "delete_kb" s (Kbt (Del r))         = Right $ Kbt $ Del $ r { delete_kb = Just s}
updateMode "delete_abbr" s (Kbt (Del r))         = Right $ Kbt $ Del $ r { delete_abbr = s}
updateMode _ _ e = Right e
