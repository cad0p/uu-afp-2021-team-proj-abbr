{-|
Description : Environment agnostic command handlers for the CLI.
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCli.Handlers where
import qualified Data.ByteString.Lazy   as BL
import           Data.Csv               (decodeByName)
import           Data.List              (intercalate)
import           Data.Maybe             (fromMaybe)
import           LibCli.Adapters        (getKnowledgeBase)
import           LibCli.OutputInterface (returnOutput)
import           LibCore.Decoder        as D (decode)
import           LibCore.KnowledgeBase  (KnowledgeBaseStructure, listAll)
import           LibCore.Mapper         as M (mapParseStructure)
import           LibCore.Models         (Error (..), Keyword (Keyword))
import           LibCore.Parser         as P (doParse)
import           System.Directory       (doesFileExist)

---------------
-- Utilities --
---------------

-- TODO: add description.
loadKb :: FilePath -> IO (Either Error KnowledgeBaseStructure)
loadKb p = do
  csvData <- BL.readFile p
  case decodeByName csvData of
    Left  s      -> return $ Left $ StandardError s
    Right (_, v) -> return $ Right $ getKnowledgeBase v

-- TODO: describe.
loadInput :: FilePath -> IO (Either Error String)
loadInput p = do
  s <- readFile p
  return $ Right s

-- TODO: describe
doExpansion :: KnowledgeBaseStructure -> String -> IO String
doExpansion kb s = do
  return $ D.decode $ M.mapParseStructure kb $ P.doParse s

--------------------------
-- Expansion Operations --
--------------------------

-- | Expand command handler.
-- Deal with single input expansion without reading an input file.
expandHandler
  :: Maybe FilePath -- ^ KB file path
  -> String -- ^ Abbreviation to expand
  -> IO () -- ^ Writes the expansion result to the STDOUT.
expandHandler kbfp abbr = do
  let kbp = fromMaybe "" kbfp
  kb_exists <- doesFileExist kbp
  res       <- process (kb_exists, kbp) abbr
  case res of
    Left  err -> error $ show err
    Right s   -> putStrLn s
 where
  process :: (Bool, FilePath) -> String -> IO (Either Error String)
  process (False, p) _ = do
    return $ Left $ StandardError $ "KB file not found at " ++ p
  process (_, kbp) s = do
    lkb <- loadKb kbp
    case (`doExpansion` s) <$> lkb of
      Left  er  -> return $ Left $ StandardError $ show er
      Right ios -> Right <$> ios


-- | Replace command handler.
-- Does all the replacing heavy-lifting.
-- Loads the required files, produces the expansions and writes the output file.
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
    Right s   -> returnOutput ofp s
 where
  -- | Connecting handling the file access and logic.
  process :: (Bool, FilePath) -> (Bool, FilePath) -> IO (Either Error String)
  process (False, p) _ = do
    return $ Left $ StandardError $ "KB file not found at " ++ p
  process _ (False, p) = do
    return $ Left $ StandardError $ "Input file not found at " ++ p
  process (_, kbp) (_, inp) = do
    lkb <- loadKb kbp
    lin <- loadInput inp
    case doExpansion <$> lkb <*> lin of
      Left  er  -> return $ Left $ StandardError $ show er
      Right ios -> Right <$> ios

-------------------------
-- Knowledge Base CRUD --
-------------------------

-- TODO:
-- add
-- update
-- delete

-- list
listHandler
  :: Maybe FilePath -- ^ KB file path
  -> IO () -- ^ Writes the full contents of the KB to the STDOUT.
listHandler kbfp = do
  let kbp = fromMaybe "" kbfp
  kb_exists <- doesFileExist kbp
  res       <- process (kb_exists, kbp)
  case res of
    Left  err -> error $ show err
    Right s   -> putStrLn s
 where
  process :: (Bool, FilePath) -> IO (Either Error String)
  process (False, p) = do
    return $ Left $ StandardError $ "KB file not found at " ++ p
  process (_, kbp) = do
    lkb <- loadKb kbp
    let rs = map formatRecord . listAll <$> lkb
    return $ intercalate "\n" <$> rs

  formatRecord :: (Keyword, Keyword) -> String
  formatRecord (Keyword kk kpl, Keyword vk vpl) =
    "Key: "
      ++ kk
      ++ (if kpl then "(plural)" else "")
      ++ " --> "
      ++ "Value: "
      ++ vk
      ++ if vpl then "(plural)" else ""


-- get

