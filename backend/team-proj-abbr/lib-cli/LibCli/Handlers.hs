{-# LANGUAGE LambdaCase #-}
{-|
Description : Environment agnostic command handlers for the CLI.
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCli.Handlers where
import           Control.Applicative    (Alternative (empty))
import qualified Data.ByteString.Lazy   as BL
import           Data.Csv               (decodeByName)
import           Data.Maybe             (fromMaybe)
import           LibCli.Adapters        (getKnowledgeBase)
import           LibCli.OutputInterface (returnOutput)
import           LibCore.Decoder        as D (decode)
import           LibCore.KnowledgeBase  (KnowledgeBaseStructure)
import           LibCore.Mapper         as M (mapParseStructure)
import           LibCore.Models         (Error (..))
import           LibCore.Parser         as P (doParse)
import           System.Directory       (doesFileExist)

---------------
-- Utilities --
---------------

-- | Wraps the file reading in a predefined error handling.
-- If the file path was not provided, returns Nothing.
-- Otherwise, executes the given function.
readWrapper
  :: (FilePath -> a) -- ^ File access function
  -> Maybe FilePath  -- ^ File path
  -> Maybe a         -- ^ Operation result
readWrapper f mfp = f <$> mfp


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
replaceHandler kbfp inpfp ofp = do
  let kbp = fromMaybe "" kbfp
  let inp = fromMaybe "" inpfp
  kb_exists  <- doesFileExist kbp
  inp_exists <- doesFileExist inp
  res        <- process (kb_exists, kbp) (inp_exists, inp)
  case res of
    Left  err -> error $ show err
    Right s   -> writeOutput ofp s
 where
  process :: (Bool, FilePath) -> (Bool, FilePath) -> IO (Either Error String)
  process (False, p) _ = do
    return $ Left $ StandardError $ "KB file not found at " ++ p
  process _ (False, p) = do
    return $ Left $ StandardError $ "Input file not found at " ++ p
  process (_, kbp) (_, inp) = do
    lkb <- loadKb kbp
    lin <- loadInput inp
    case exec <$> lkb <*> lin of
      Left  er  -> return $ Left $ StandardError $ show er
      Right ios -> Right <$> ios

  loadKb :: FilePath -> IO (Either Error KnowledgeBaseStructure)
  loadKb p = do
    putStrLn p
    csvData <- BL.readFile p
    case decodeByName csvData of
      Left  s      -> return $ Left $ StandardError s
      Right (_, v) -> return $ Right $ getKnowledgeBase v

  loadInput :: FilePath -> IO (Either Error String)
  loadInput _ = do
    return $ Left $ StandardError "Load input is not implemented"

  exec :: KnowledgeBaseStructure -> String -> IO String
  exec kb s = do
    return $ D.decode $ M.mapParseStructure kb $ P.doParse s

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

