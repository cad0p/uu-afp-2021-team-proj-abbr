{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import qualified System.Console.CmdArgs        as CMD

projectName :: String
projectName = "shrthdnr"

-- |ShortHndr CLI interface specification.
data ShortHndr
  = Expand
      { input   :: Maybe FilePath
      , out     :: Maybe FilePath
      , inplace :: Maybe Bool
      , kb      :: Maybe FilePath
      }
  | Lookup
      { abbreviation :: String
      , kb           :: Maybe FilePath
      }
  | List
      { kb :: Maybe FilePath
      }
  | Add
      { keyword   :: String
      , expansion :: String
      , kb              :: Maybe FilePath
      }
  -- TODO: support additional detail updates later
  | Update
      { keyword   :: String
      , expansion :: String
      , kb        :: Maybe FilePath
      }
  | Delete
      { keyword :: String
      , kb      :: Maybe FilePath
      }
  deriving (CMD.Data, Show, CMD.Typeable)

fileFlags :: String -> Maybe FilePath -> Maybe FilePath
fileFlags h f = f CMD.&= CMD.help h CMD.&= CMD.typFile

-------------------------
-- Expansion commands: --
-------------------------

expand :: ShortHndr
expand =
    Expand
            { input   = fileFlags "Source file" (pure "shorthndr-input.txt")
            , out     = fileFlags "Output file" (pure "shorthndr--out.txt")
            , kb      = fileFlags "KnowledgeBase source file"
                                  (pure "shorthndr-kb.csv")
            , inplace = CMD.def
            }
        CMD.&= CMD.help "Expand all abreviations in the provided file"

lookup :: ShortHndr
lookup =
    Lookup
            { abbreviation = CMD.def
            , kb           = fileFlags "KnowledgeBase source file"
                                       (pure "shorthndr-kb.csv")
            }
        CMD.&= CMD.help "Expand all abreviations in the provided file"


------------------------------
-- Knowledge Base commands: --
------------------------------

add :: ShortHndr
add =
    Add { keyword = CMD.def
        , expansion = CMD.def
        , kb = fileFlags "KnowledgeBase source file" (pure "shorthndr-kb.csv")
        }
        CMD.&= CMD.help "Add a new abbreviation record to the Knowledge Base"

-- |Main entrypoint of the CLI application.
--
-- Endpoint design:
-- TODO:
-- [ ]     - expand - expand all the abbreviation in the full text file
-- [ ]     - lookup - find an expansion for a single abbreviation input
-- [ ]     - list   - list all the known expansion records
-- [ ]     - add    - add a new record to the knowledge base
-- [ ]     - update - modify an existing record in the knowledge base
-- [ ]     - delete - delete an existing record from the knowledge base
main :: IO ()
main = mockCliHandler =<< CMD.cmdArgs (CMD.modes [expand, add])
    where mockCliHandler = print
