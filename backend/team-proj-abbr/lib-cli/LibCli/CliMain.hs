module LibCli.CliMain where

import qualified LibCli.CliSpec         as CS (ShortHndr (..), cliModes)
import qualified System.Console.CmdArgs as CMD

-----------------------
-- Command Handlers: --
-----------------------

-- TODO(tech-debt): define a typeclass for the modes instead of the pattern matching
-- TODO: (future task) implement the actual handlers with the business logic.
mockCliHandler :: CS.ShortHndr -> IO ()
mockCliHandler c@CS.Replace{} = print $ "replacing! --> " ++ show c
mockCliHandler c@CS.Expand{}  = print $ "expanding! --> " ++ show c
mockCliHandler c@CS.List{}    = print $ "listing! --> " ++ show c
mockCliHandler c@CS.Add{}     = print $ "adding! --> " ++ show c
mockCliHandler c@CS.Update{}  = print $ "updating! --> " ++ show c
mockCliHandler c@CS.Delete{}  = print $ "deleting! --> " ++ show c

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
--    * See 'LibCli.CliSpec' for more information about the CLI endpoints.
cliMain :: IO ()
cliMain = mockCliHandler =<< CMD.cmdArgs (CMD.modes CS.cliModes)
