{-# LANGUAGE LambdaCase #-}
{-|
Description : Command Line Interface - Main
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}


module LibCli.Main where

import qualified Data.ByteString.Lazy    as BL
import           Data.Csv                (decodeByName)
import           LibCli.Spec             (ShortHndr (input, kb, out))
import qualified LibCli.Spec             as CS (ShortHndr (..), cliModes)
import           LibCore.Decoder         (decode)
import           LibCore.KnowledgeBase   (getKnowledgeBase)
import           LibCore.Mapper          (mapParseStructure)
import           LibCore.OutputInterface (returnOutput)
import           LibCore.Parser          (doParse)
import qualified System.Console.CmdArgs  as CMD
import           System.Directory        (doesFileExist)


-----------------------
-- Command Handlers: --
-----------------------

-- TODO(tech-debt): define a typeclass for the modes instead of the pattern matching
-- TODO: (future task) implement the actual handlers with the business logic.
mockCliHandler :: CS.ShortHndr -> IO ()
-- mockCliHandler c@CS.Replace{} = print $ "replacing! --> " ++ show c
mockCliHandler c@CS.Replace{} = replaceMode c
mockCliHandler c@CS.Expand{}  = print $ "expanding! --> " ++ show c
mockCliHandler c@CS.List{}    = print $ "listing! --> " ++ show c
mockCliHandler c@CS.Add{}     = print $ "adding! --> " ++ show c
mockCliHandler c@CS.Update{}  = print $ "updating! --> " ++ show c
mockCliHandler c@CS.Delete{}  = print $ "deleting! --> " ++ show c


{- 'replaceMode' does the replacind heavy lifting

  Under the hood it checks for errors in the input file (TODO input file) and the KB
-}
replaceMode :: ShortHndr -> IO ()
replaceMode c@CS.Replace{} = do
  case input c of
    Nothing   -> error "No input FilePath was found"
    -- TODO from Pier to Wilmer: this error above is never reached
    Just i_fp -> do
      s <- readFile i_fp
      case kb c of
        Nothing    -> error "No KB FilePath was found"
        -- source: https://stackoverflow.com/questions/16952335/is-there-any-way-to-use-io-bool-in-if-statement-without-binding-to-a-name-in-has
        Just kb_fp -> doesFileExist kb_fp >>= \case
          False -> error ("The KB File '" ++ kb_fp ++ "' does not exist")
          True  -> do
            putStrLn kb_fp
            csvData <- BL.readFile kb_fp
            case decodeByName csvData of
              Left  err       -> error ("decoding error: " ++ err)
              Right (_, kb_v) -> returnOutput
                (out c)
                (decode (mapParseStructure (getKnowledgeBase kb_v) (doParse s)))
-- Impossible case because of the mockCliHandler
replaceMode _ = undefined

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
cliMain = mockCliHandler =<< CMD.cmdArgs (CMD.modes CS.cliModes)
