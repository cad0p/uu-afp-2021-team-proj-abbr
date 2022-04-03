{-|
Description : Command Line Interface - Main
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}


module LibCli.Main where

import           LibCli.Spec
import           LibCore.Decoder                 (decode)
import           LibCore.KnowledgeBase           (getKnowledgeBase)
import           LibCore.Mapper                  (mapParseStructure)
import           LibCore.OutputInterface         (returnOutput)
import           LibCore.Parser                  (doParse)
import           System.Console.CmdArgs.Explicit
    ( HelpFormat (HelpFormatDefault)
    , helpText
    , processArgs
    )

-----------------------
-- Command Handlers: --
-----------------------

-- TODO: (future task) implement the actual handlers with the business logic.
mockCliHandler :: ShortHndrModes -> IO ()
mockCliHandler (Exp e) = handleExpMode e
mockCliHandler (Kbt k) = handleKbtMode k
mockCliHandler Hlp     = print $ helpText [] HelpFormatDefault arguments

handleExpMode :: Expansion -> IO ()
handleExpMode (Re r) = replaceMode r
handleExpMode (Ex c) = print $ "expanding! --> " ++ show c

handleKbtMode :: KnowledgeBaseTypes -> IO ()
handleKbtMode (Lst c) = print $ "listing! --> " ++ show c
handleKbtMode (Ad c)  = print $ "adding! --> " ++ show c
handleKbtMode (Up c)  = print $ "updating! --> " ++ show c
handleKbtMode (Del c) = print $ "deleting! --> " ++ show c

replaceMode :: Replace -> IO ()
replaceMode c = do
    case input c of
      Nothing -> error "No input file was found"
      Just f -> do
          s <- readFile f
          returnOutput (out c) (decode $ mapParseStructure getKnowledgeBase $ doParse s)

----------------------------
-- Executable entrypoiny: --
----------------------------

-- |Main entrypoint of the CLI application.
--
-- /CLI endpoints:/
--
--    * replace   - expand all the abbreviation in the full text file
--    * expand    - find an expansion for a single abbreviation input
--    * list      - list all the known expansion records
--    * add       - add a new record to the knowledge base
--    * update    - modify an existing record in the knowledge base
--    * delete    - delete an existing record from the knowledge base
--
-- /Usage:/
--
--    * --help                            - prints the full CLI tool support
--    * \<command\> [args\*] [options\*]  - invokes the command
--
-- /See more:/
--
--    * See 'LibCli.Spec' for more information about the CLI endpoints.
cliMain :: IO ()
cliMain = do
       xs <- processArgs arguments
       case xs of
         Exp ex  -> handleExpMode ex
         Kbt kbt -> print kbt
         Hlp     -> print $ helpText [] HelpFormatDefault arguments
