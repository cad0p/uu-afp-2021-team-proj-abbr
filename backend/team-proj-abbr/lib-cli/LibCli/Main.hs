{-|
Description : Command Line Interface - Main
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}


module LibCli.Main where

import           LibCli.Handlers
    ( addHandler
    , deleteHandler
    , expandHandler
    , listHandler
    , replaceHandler
    )
import           LibCli.Spec
    ( ShortHndr (abbreviation, expansion, input, kb, out)
    )
import qualified LibCli.Spec            as CS (ShortHndr (..), cliModes)
import qualified System.Console.CmdArgs as CMD


-----------------------
-- Command Handlers: --
-----------------------

-- TODO(tech-debt): define a typeclass for the modes instead of the pattern matching
cliController :: CS.ShortHndr -> IO ()
-- Expansion commands
cliController CS.Replace { input = ifp, kb = kbfp, out = ofp } =
  replaceHandler kbfp ifp ofp
cliController CS.Expand { abbreviation = abbr, kb = kbfp } =
  expandHandler kbfp abbr
-- CRUD commands
cliController CS.List { kb = kbfp } = listHandler kbfp
cliController CS.Add { kb = kbfp, abbreviation = a, expansion = e } =
  addHandler kbfp a e
cliController c@CS.Update{} = print $ "updating! --> " ++ show c
cliController CS.Delete { kb = kbfp, abbreviation = a } = deleteHandler kbfp a

----------------------------
-- Executable entrypoint: --
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
cliMain = cliController =<< CMD.cmdArgs (CMD.modes CS.cliModes)
