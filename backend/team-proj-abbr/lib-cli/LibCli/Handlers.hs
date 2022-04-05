{-|
Description : Environment agnostic command handlers for the CLI.
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCli.Handlers where
import           LibCli.OutputInterface (returnOutput)
import           LibCore.Decoder        as D (decode)
import           LibCore.KnowledgeBase  (KnowledgeBaseStructure)
import           LibCore.Mapper         as M (mapParseStructure)
import           LibCore.Models         (Error (..))
import           LibCore.Parser         as P (doParse)

---------------
-- Utilities --
---------------

-- | Wraps the file loading in a predefined error handling.
readWrapper :: (FilePath -> a) -> Maybe FilePath -> Either Error a
readWrapper f mfp = case mfp of
  Nothing -> Left $ StandardError "no file path provided."
  Just s  -> Right $ f s

--------------------------
-- Expansion Operations --
--------------------------

-- TODO:
-- expand

-- | Replace command handler.
replaceHandler
  :: Maybe FilePath -- ^ KB file path
  -> Maybe FilePath -- ^ Input file path
  -> Maybe FilePath -- ^ Output file path
  -> IO () -- ^ Writes the modified file out to the specified location
replaceHandler kbfp ifp ofp = do
  case process of
    Left  err -> error $ show err
    Right s   -> writeOutput ofp s
 where
  process :: Either Error String
  process = exec <$> loadKb kbfp <*> loadInput ifp

  loadKb :: Maybe FilePath -> Either Error KnowledgeBaseStructure
  loadKb = const $ Left $ StandardError "not implemented KB"

  loadInput :: Maybe FilePath -> Either Error String
  loadInput = const $ Left $ StandardError "not implemented INPUT"

  exec :: KnowledgeBaseStructure -> String -> String
  exec kb s = D.decode $ M.mapParseStructure kb $ P.doParse s

  writeOutput :: Maybe FilePath -> String -> IO ()
  writeOutput = returnOutput


-------------------------
-- Knowledge Base CRUD --
-------------------------

-- TODO:
-- add
-- update
-- delete
-- list
-- get

