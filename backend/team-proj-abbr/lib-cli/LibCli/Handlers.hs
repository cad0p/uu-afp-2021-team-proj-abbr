{-|
Description : Environment agnostic command handlers for the CLI.
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCli.Handlers where
import qualified Data.ByteString.Lazy  as BL (readFile, writeFile)
import           Data.Csv
    ( decodeByName
    , encodeDefaultOrderedByName
    )
import           Data.List             (intercalate)
import           Data.Maybe            (fromMaybe)
import           LibCli.Adapters       (getKnowledgeBase, mapKeywordPair)
import           LibCore.Decoder       as D (decode)
import           LibCore.KnowledgeBase
    ( KnowledgeBaseStructure
    , add
    , listAll
    , put
    , remove
    )
import           LibCore.Mapper        as M (mapParseStructure)
import           LibCore.Models        (Error (..), Keyword (..))
import           LibCore.Parser        as P (doParse)
import           System.Directory      (doesFileExist)
import           System.IO.Strict      as SIS (readFile)


-- TODO: General improvements (tech debt):
--  [ ] refactor duplication
--  [ ] try to use nice template for handlers to make them shorter
--  [ ] introduce a separate Utilities module for easier testing


---------------
-- Utilities --
---------------

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
formatRecord (Keyword kk kpl, Keyword vk vpl) =
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


-- | Embeds the string in a keyword.
-- By default assigns `False` to the plural attribute.
--
-- Examples:
--
-- >>> makeDefaultKeyword "hello"
-- Keyword {keyword = "hello", plural = False}
makeDefaultKeyword :: String -> Keyword
makeDefaultKeyword s = Keyword { keyword = s, plural = False }


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

-- | Add command handler.
-- Adds new abbreviation with the associated expansion to the KB.
addHandler
  :: Maybe FilePath -- ^ KB file path
  -> String -- ^ Abbreviation keyword
  -> String -- ^ Expansion keyword
  -> IO () -- ^ Writes the full contents of the KB to the STDOUT.
addHandler kbfp a e = do
  let kbp = fromMaybe "" kbfp
  kb_exists <- doesFileExist kbp
  res       <- process (kb_exists, kbp)
  case res of
    Left  err     -> error $ show err
    Right (s, kb) -> do
      putStrLn s
      dump kbp kb
 where
  process
    :: (Bool, FilePath) -> IO (Either Error (String, KnowledgeBaseStructure))
  process (False, p) = do
    return $ Left $ StandardError $ "KB file not found at " ++ p
  process (_, kbp) = do
    lkb <- loadKb kbp
    let k   = pure $ makeDefaultKeyword a
    let v   = pure $ makeDefaultKeyword e
    let res = add <$> lkb <*> k <*> v
    case res of
      Left er ->
        return $ Left $ StandardError $ "Could not add new keyword: " ++ show er
      Right (nk, kb') -> do
        return $ pure ("Added: " ++ show nk, kb')


-- | Update command handler.
-- Updates an existing abbreviation in the KB.
updateHandler
  :: Maybe FilePath -- ^ KB file path
  -> String -- ^ Abbreviation keyword
  -> String -- ^ Expansion keyword
  -> IO () -- ^ Writes the full contents of the KB to the STDOUT.
updateHandler kbfp a e = do
  let kbp = fromMaybe "" kbfp
  kb_exists <- doesFileExist kbp
  res       <- process (kb_exists, kbp)
  case res of
    Left  err     -> error $ show err
    Right (s, kb) -> do
      putStrLn s
      dump kbp kb
 where
  process
    :: (Bool, FilePath) -> IO (Either Error (String, KnowledgeBaseStructure))
  process (False, p) = do
    return $ Left $ StandardError $ "KB file not found at " ++ p
  process (_, kbp) = do
    lkb <- loadKb kbp
    let k = pure $ makeDefaultKeyword a
    let v = pure $ makeDefaultKeyword e
    let s = (\k' v' -> "Updated: " ++ show k' ++ " to " ++ show v') <$> k <*> v
    case put <$> lkb <*> k <*> v of
      Left  er -> error $ show er
      Right r  -> return $ (\x (_, y) -> (x, y)) <$> s <*> r


-- | Delete command handler.
-- Deletes an existing abbreviation from the KB.
deleteHandler
  :: Maybe FilePath -- ^ KB file path
  -> String -- ^ Abbreviation keyword
  -> IO () -- ^ Writes the full contents of the KB to the STDOUT.
deleteHandler kbfp a = do
  let kbp = fromMaybe "" kbfp
  kb_exists <- doesFileExist kbp
  res       <- process (kb_exists, kbp)
  case res of
    Left  err     -> error $ show err
    Right (s, kb) -> do
      putStrLn s
      dump kbp kb
 where
  process
    :: (Bool, FilePath) -> IO (Either Error (String, KnowledgeBaseStructure))
  process (False, p) = do
    return $ Left $ StandardError $ "KB file not found at " ++ p
  process (_, kbp) = do
    lkb <- loadKb kbp
    let k = pure $ makeDefaultKeyword a
    let s = (\k' -> "Removed: " ++ show k') <$> k
    case remove <$> lkb <*> k of
      Left  er -> error $ show er
      Right e  -> return $ (,) <$> s <*> e


-- | List command handler.
-- Displays all the contents of the specified Knowledge base.
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


-- TODO(tech debt): add get command to Spec
-- get
