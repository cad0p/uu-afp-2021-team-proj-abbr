{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           System.Console.CmdArgs

projectName :: String
projectName = "shrthdnr"

-- |ShortHndr CLI interface specification.
data ShortHndr
  = ExpandBulk
      { src     :: Maybe FilePath
      , out     :: Maybe FilePath
      , inplace :: Maybe Bool
      }
  | AddRecord
      { keyword   :: String
      , expansion :: String
      , src       :: Maybe FilePath
      }
  deriving (Data, Show, Typeable)

fileFlags :: String -> Maybe FilePath -> Maybe FilePath
fileFlags h f = f &= help h &= typFile

expandBulk :: ShortHndr
expandBulk = ExpandBulk
    {src = fileFlags "Source file" (pure "shorthndr-input.txt")
    ,out = fileFlags "Output file" (pure "shorthndr--out.txt")
    ,inplace = def
    } &= help "Expand the abreviations in the provided file"

addRecord :: ShortHndr
addRecord = AddRecord
    {keyword = def
    ,expansion = def
    ,src = fileFlags "KnowledgeBase source file" (pure "shorthndr-kb.csv")
    } &= help "Add a new abbreviation record to the Knowledge Base"

-- |Main entrypoint of the CLI application.
--
-- Endpoint design:
--      - expand - expand all the abbreviation in the full text file
--      - lookup - find an expansion for a single abbreviation input
--      - list   - list all the known expansion records
--      - add    - add a new record to the knowledge base
--      - update - modify an existing record in the knowledge base
--      - delete - delete an existing record from the knowledge base
main :: IO ()
main = mockCliHandler =<< cmdArgs (modes [expandBulk, addRecord])
  where mockCliHandler = print
