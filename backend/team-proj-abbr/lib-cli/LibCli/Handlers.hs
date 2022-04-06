{-|
Description : Environment agnostic command handlers for the CLI.
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

-- template for haddock docs taken from here:
-- https://hackage.haskell.org/package/directory-1.3.7.0/docs/src/System.Directory.html
module LibCli.Handlers
  (
  -- $intro

  -- * Utilities
    loadKb
  , loadInput
  , doExpansion
  , formatRecord
  , dump
  , makeDefaultKeyword
  -- * Expansion Operations

  -- $expansion
  , expandHandler
  , replaceHandler
  -- * Knowledge Base CRUD
  , addHandler
  , updateHandler
  , deleteHandler
  , listHandler
  ) where
import qualified Data.ByteString.Lazy   as BL (readFile, writeFile)
import           Data.Csv
    ( decodeByName
    , encodeDefaultOrderedByName
    )
import           Data.List              (intercalate)
import           Data.Maybe             (fromMaybe)
import           LibCli.Adapters        (getKnowledgeBase, mapKeywordPair)
import           LibCli.OutputInterface (returnOutput)
import           LibCore.Decoder        as D (decode)
import           LibCore.KnowledgeBase
    ( KnowledgeBaseStructure
    , add
    , listAll
    , put
    , remove
    )
import           LibCore.Mapper         as M (mapParseStructure)
import           LibCore.Models         (Error (..), Keyword (..))
import           LibCore.Parser         as P (doParse)
import           System.Directory       (doesFileExist)
import           System.IO.Strict       as SIS (readFile)


-- TODO: General improvements (tech debt):
--  [ ] refactor duplication
--  [ ] try to use nice template for handlers to make them shorter

{- $intro

Handlers are the core part of the CLI, as they handle the commands
 and how they should behave.
-}

------------------------------------
-- Expansion Operations

{- $expansion

Here we adopt the following convention:

* `fp` is a `FilePath`

* `m` stands for `Maybe`, so `mfp` is `Maybe FilePath`

What precedes is the following:

* `kb` is the `KnowledgeBaseStructure`
  and if followed by `_fp` it means that the underlying
  contents of the `File` at `FilePath` `fp` is a `kb`

* in is the same, input file

* `o` is output file

* `io` is input/output file (used in `inplace` mode)

-}


---------------
-- Utilities --
---------------

-- | Function to load the Knowledge Base from the specified file.
-- Supports only CSV files.
loadKb :: FilePath -> IO (Either Error KnowledgeBaseStructure)
loadKb fp = do
  csvData <- BL.readFile fp
  case decodeByName csvData of
    Left  s      -> return $ Left $ StandardError s
    Right (_, v) -> return $ Right $ getKnowledgeBase v

-- | Function to load the abbreviation input file.
-- Can be any file with contents loadable as String.
loadInput :: FilePath -> IO (Either Error String)
loadInput fp = do
  s <- SIS.readFile fp
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
dump kb_fp kb = do
  let entries = map mapKeywordPair $ listAll kb
  BL.writeFile kb_fp $ encodeDefaultOrderedByName entries


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
  let kb_fp = fromMaybe "" kbfp
  kb_exists <- doesFileExist kb_fp
  res       <- process (kb_exists, kb_fp) abbr
  case res of
    Left  err -> error $ show err
    Right s   -> putStrLn s
 where
  process :: (Bool, FilePath) -> String -> IO (Either Error String)
  process (False, fp) _ = do
    return $ Left $ StandardError $ "KB file not found at " ++ fp
  process (_, kb_fp) s = do
    lkb <- loadKb kb_fp
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
replaceHandler kb_mfp in_mfp o_mfp = do
  -- here m stands for maybe, fp stands for FilePath
  let kb_fp = fromMaybe "" kb_mfp
  let inp   = fromMaybe "" in_mfp
  kb_exists  <- doesFileExist kb_fp
  inp_exists <- doesFileExist inp
  res        <- process (kb_exists, kb_fp) (inp_exists, inp)
  case res of
    Left  err -> error $ show err
    Right s   -> returnOutput o_mfp s
 where
  -- | Connecting handling the file access and logic.
  process :: (Bool, FilePath) -> (Bool, FilePath) -> IO (Either Error String)
  process (False, fp) _ = do
    return $ Left $ StandardError $ "KB file not found at " ++ fp
  process _ (False, fp) = do
    return $ Left $ StandardError $ "Input file not found at " ++ fp
  process (_, kb_fp) (_, inp) = do
    lkb <- loadKb kb_fp
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
  let kb_fp = fromMaybe "" kbfp
  kb_exists <- doesFileExist kb_fp
  res       <- process (kb_exists, kb_fp)
  case res of
    Left  err     -> error $ show err
    Right (s, kb) -> do
      putStrLn s
      dump kb_fp kb
 where
  process
    :: (Bool, FilePath) -> IO (Either Error (String, KnowledgeBaseStructure))
  process (False, fp) = do
    return $ Left $ StandardError $ "KB file not found at " ++ fp
  process (_, kb_fp) = do
    lkb <- loadKb kb_fp
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
  let kb_fp = fromMaybe "" kbfp
  kb_exists <- doesFileExist kb_fp
  res       <- process (kb_exists, kb_fp)
  case res of
    Left  err     -> error $ show err
    Right (s, kb) -> do
      putStrLn s
      dump kb_fp kb
 where
  process
    :: (Bool, FilePath) -> IO (Either Error (String, KnowledgeBaseStructure))
  process (False, fp) = do
    return $ Left $ StandardError $ "KB file not found at " ++ fp
  process (_, kb_fp) = do
    lkb <- loadKb kb_fp
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
  let kb_fp = fromMaybe "" kbfp
  kb_exists <- doesFileExist kb_fp
  res       <- process (kb_exists, kb_fp)
  case res of
    Left  err     -> error $ show err
    Right (s, kb) -> do
      putStrLn s
      dump kb_fp kb
 where
  process
    :: (Bool, FilePath) -> IO (Either Error (String, KnowledgeBaseStructure))
  process (False, fp) = do
    return $ Left $ StandardError $ "KB file not found at " ++ fp
  process (_, kb_fp) = do
    lkb <- loadKb kb_fp
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
  let kb_fp = fromMaybe "" kbfp
  kb_exists <- doesFileExist kb_fp
  res       <- process (kb_exists, kb_fp)
  case res of
    Left  err -> error $ show err
    Right s   -> putStrLn s
 where
  process :: (Bool, FilePath) -> IO (Either Error String)
  process (False, fp) = do
    return $ Left $ StandardError $ "KB file not found at " ++ fp
  process (_, kb_fp) = do
    lkb <- loadKb kb_fp
    let rs = map formatRecord . listAll <$> lkb
    return $ intercalate "\n" <$> rs


-- TODO(tech debt): add get command to Spec
-- get
