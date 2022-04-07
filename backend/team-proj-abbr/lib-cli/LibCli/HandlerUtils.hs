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

import           LibCli.Adapters       (getKnowledgeBase, mapKeywordPair)
import           LibCore.Decoder       as D (decode)
import           LibCore.KnowledgeBase (KnowledgeBaseStructure, listAll)
import           LibCore.Mapper        as M (mapParseStructure)
import           LibCore.Models        (AKeyword (..), Error (..), Keyword)
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
formatRecord (KeywordBody kk kpl, KeywordBody vk vpl) =
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
returnOutput :: Maybe FilePath -> String -> IO ()
returnOutput Nothing   = error "No output file found"
returnOutput (Just fp) = writeFile fp
