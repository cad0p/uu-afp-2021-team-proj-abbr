{-|
Description : Various utilities used by the CLI Command Handlers.
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCli.HandlerUtils where

import qualified Data.ByteString.Lazy  as BL (readFile, writeFile)
import           Data.Csv
    ( decodeByName
    , encodeDefaultOrderedByName
    )

import qualified Control.Exception     as Exc
import           LibCli.Adapters       (getKnowledgeBase, mapKeywordPair)
import           LibCore.Decoder       as D (decode)
import           LibCore.KnowledgeBase (KnowledgeBaseStructure, listAll)
import           LibCore.Mapper        as M (mapParseStructure)
import           LibCore.Models
    ( AKeyword (..)
    , Error (StandardError)
    , Keyword
    )
import           LibCore.Parser        as P (doParse)
import           System.IO.Strict      as SIS (readFile)



-- | Function to load the Knowledge Base from the specified file.
-- Supports only CSV files.
loadKb :: FilePath -> IO (Either Error KnowledgeBaseStructure)
loadKb p = do
  csvData <- BL.readFile p
  case decodeByName csvData of
    Left  s      -> return $ Left $ StandardError s
    Right (_, v) -> return $ Right $ getKnowledgeBase v


-- | Function to load the abbreviation input file.
-- Can be any file with contents loadable as String.
loadInput :: FilePath -> IO (Either Error String)
loadInput p = do
  s <- SIS.readFile p
  return $ Right s


-- | Performs the expansion logic on the provided string.
doExpansion :: KnowledgeBaseStructure -> String -> IO String
doExpansion kb s = do
  return $ D.decode $ M.mapParseStructure kb $ P.doParse s


-- | Pretty prints the Keyword pair (key, value) from the Knowledge Base
formatRecord :: (Keyword, Keyword) -> String
formatRecord (Key kk kpl, Key vk vpl) =
  "Key: "
    ++ kk
    ++ (if kpl then "(plural)" else "")
    ++ " --> "
    ++ "Value: "
    ++ vk
    ++ if vpl then "(plural)" else ""


-- | Dumps the Knowledge Base to the specified file path.
-- Writes the KB out in CSV format.
dump :: FilePath -> KnowledgeBaseStructure -> IO ()
dump kbp kb = do
  let entries = map mapKeywordPair $ listAll kb
  BL.writeFile kbp $ encodeDefaultOrderedByName entries


-- | Given a FilePath and a string, write the string to the FilePath.
-- Throws an error if no valid path is specified.
-- Write the given string to the file path otherwise.
returnOutput :: FilePath -> String -> IO ()
returnOutput = writeFile

----------------------------
-- File path access rules --
----------------------------

-- |Gets the pair of paths to use depending on the flag.
-- Returns an error if a pair of (Input, Output) file paths is not possible.
-- Otherwise, returns the pair of file paths in the following order: (Input, Output).
getInOutFilePaths
  :: Maybe FilePath -- ^ Input file path
  -> Maybe FilePath -- ^ Output file path
  -> Bool           -- ^ Whether working in in-place mode.
  -> Either Error (FilePath, FilePath) -- ^ Either error or the correct (Input, Output) pair.
getInOutFilePaths Nothing _ _ =
  Left $ StandardError "No input file path is specified"
getInOutFilePaths (Just _) (Just _) True = Left $ StandardError
  "With `inplace`, only input or output file path must be specified, not both"
getInOutFilePaths (Just _) Nothing False =
  Left $ StandardError "No output file path is specified"
getInOutFilePaths (Just in_fp) (Just o_fp) False = Right (in_fp, o_fp)
getInOutFilePaths (Just in_fp) Nothing     True  = Right (in_fp, in_fp)

-- | Handles the retrieval of Knowledge Base file paths.
-- If nothing is provided, raises an error.
-- Otherwise, returns the given file path.
getKnowledgeBaseFilePath :: Maybe FilePath -> Either Error FilePath
getKnowledgeBaseFilePath Nothing =
  Left $ StandardError "Knowledge base path must be specified"
getKnowledgeBaseFilePath (Just fp) = Right fp


--------------------
-- Error handling --
--------------------

-- | Returns the value from a potentially failing computation.
--
-- Abort with the error message if it was an error.
--
-- The text message is added to the 'Error' as additional context before aborting.
--
-- __Panics:__ if Error
--
-- Example:
--
-- >>> unwrapIOError $ Left (StandardError "oh no!")
-- *** Exception: StandardError "oh no!"
--
-- >>> unwrapIOError $ Right 42
-- 42
--
unwrapIOError :: Either Error a -> IO a
unwrapIOError (Left  err) = Exc.throwIO err
unwrapIOError (Right a  ) = pure a
