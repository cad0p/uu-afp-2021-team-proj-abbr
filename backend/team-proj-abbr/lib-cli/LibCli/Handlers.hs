{-|
Description : CLI framework agnostic command handlers for the CLI.
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

  -- * Expansion Operations

  -- $expansion
    expandHandler
  , replaceHandler

  -- * Knowledge Base CRUD
  , addHandler
  , updateHandler
  , deleteHandler
  , listHandler
  ) where

import           Data.List             (intercalate)
import           LibCli.HandlerUtils
    ( doExpansion
    , dump
    , formatRecord
    , getInOutFilePaths
    , getKnowledgeBaseFilePath
    , loadInput
    , loadKb
    , returnOutput
    , unwrapIOError
    )
import           LibCore.KnowledgeBase
    ( KnowledgeBaseStructure
    , add
    , listAll
    , put
    , remove
    )
import           LibCore.Models        (Error (..))
import           System.Directory      (doesFileExist)

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

* TODO `io` is input/output file (used in `inplace` mode)

-}

-- TODO: General improvements (tech debt):
--  [ ] refactor duplication
--  [ ] try to use nice template for handlers to make them shorter

-- | Expand command handler.
-- Deal with single input expansion without reading an input file.
expandHandler
  :: Maybe FilePath -- ^ KB file path
  -> String -- ^ Abbreviation to expand
  -> IO () -- ^ Writes the expansion result to the STDOUT.
expandHandler kb_mfp abbr = do
  kb_fp     <- unwrapIOError $ getKnowledgeBaseFilePath kb_mfp
  kb_exists <- doesFileExist kb_fp
  res       <- process (kb_exists, kb_fp) abbr
  unwrapIOError res >>= putStrLn
 where
  process :: (Bool, FilePath) -> String -> IO (Either Error String)
  process (False, fp) _ = do
    return $ Left $ StandardError $ "KB file not found at " ++ fp
  process (_, kb_fp) s = do
    lkb <- loadKb kb_fp
    unwrapIOError ((`doExpansion` s) <$> lkb) >>= fmap Right


-- | Replace command handler.
-- Does all the replacing heavy-lifting.
-- Loads the required files, produces the expansions and writes the output file.
replaceHandler
  :: Maybe FilePath -- ^ KB file path
  -> Maybe FilePath -- ^ Input file path
  -> Maybe FilePath -- ^ Output file path
  -> Bool  -- ^ Whether inplace mode is used or not
  -> IO () -- ^ Writes the modified file out to the specified location
replaceHandler kb_mfp in_mfp o_mfp in_mode = do
  kb_fp         <- unwrapIOError $ getKnowledgeBaseFilePath kb_mfp
  (in_fp, o_fp) <- unwrapIOError $ getInOutFilePaths in_mfp o_mfp in_mode
  kb_exists     <- doesFileExist kb_fp
  in_exists     <- doesFileExist in_fp
  res           <- process (kb_exists, kb_fp) (in_exists, in_fp)
  unwrapIOError res >>= returnOutput o_fp
 where
  process :: (Bool, FilePath) -> (Bool, FilePath) -> IO (Either Error String)
  process (False, fp) _ = do
    return $ Left $ StandardError $ "KB file not found at " ++ fp
  process _ (False, fp) = do
    return $ Left $ StandardError $ "Input file not found at " ++ fp
  process (_, kb_fp) (_, in_fp) = do
    lkb <- loadKb kb_fp
    lin <- loadInput in_fp
    unwrapIOError (doExpansion <$> lkb <*> lin) >>= fmap Right

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
addHandler kb_mfp a e = do
  kb_fp     <- unwrapIOError $ getKnowledgeBaseFilePath kb_mfp
  kb_exists <- doesFileExist kb_fp
  res       <- process (kb_exists, kb_fp)
  (s, kb)   <- unwrapIOError res
  putStrLn s
  dump kb_fp kb
 where
  process
    :: (Bool, FilePath) -> IO (Either Error (String, KnowledgeBaseStructure))
  process (False, fp) = do
    return $ Left $ StandardError $ "KB file not found at " ++ fp
  process (_, kb_fp) = do
    lkb <- loadKb kb_fp
    let k   = pure $ pure a
    let v   = pure $ pure e
    let res = add <$> lkb <*> k <*> v
    case res of
      -- Pier note: here something like this could be implemented:
      -- https://hackage.haskell.org/package/error-0.3.0.0/docs/Data-Error.html#v:errorContext
      Left err ->
        return
          $  Left
          $  StandardError
          $  "Could not add new keyword: "
          ++ show err
      Right (nk, kb') -> do
        return $ pure ("Added: " ++ show nk, kb')


-- | Update command handler.
-- Updates an existing abbreviation in the KB.
updateHandler
  :: Maybe FilePath -- ^ KB file path
  -> String -- ^ Abbreviation keyword
  -> String -- ^ Expansion keyword
  -> IO () -- ^ Writes the full contents of the KB to the STDOUT.
updateHandler kb_mfp a e = do
  kb_fp     <- unwrapIOError $ getKnowledgeBaseFilePath kb_mfp
  kb_exists <- doesFileExist kb_fp
  res       <- process (kb_exists, kb_fp)
  (s, kb)   <- unwrapIOError res
  putStrLn s
  dump kb_fp kb
 where
  process
    :: (Bool, FilePath) -> IO (Either Error (String, KnowledgeBaseStructure))
  process (False, fp) = do
    return $ Left $ StandardError $ "KB file not found at " ++ fp
  process (_, kb_fp) = do
    lkb <- loadKb kb_fp
    let k = pure $ pure a
    let v = pure $ pure e
    let s = (\k' v' -> "Updated: " ++ show k' ++ " to " ++ show v') <$> k <*> v
    r <- unwrapIOError (put <$> lkb <*> k <*> v)
    return $ (\x (_, y) -> (x, y)) <$> s <*> r


-- | Delete command handler.
-- Deletes an existing abbreviation from the KB.
deleteHandler
  :: Maybe FilePath -- ^ KB file path
  -> String -- ^ Abbreviation keyword
  -> IO () -- ^ Writes the full contents of the KB to the STDOUT.
deleteHandler kb_mfp a = do
  kb_fp     <- unwrapIOError $ getKnowledgeBaseFilePath kb_mfp
  kb_exists <- doesFileExist kb_fp
  res       <- process (kb_exists, kb_fp)
  (s, kb)   <- unwrapIOError res
  putStrLn s
  dump kb_fp kb

 where
  process
    :: (Bool, FilePath) -> IO (Either Error (String, KnowledgeBaseStructure))
  process (False, fp) = do
    return $ Left $ StandardError $ "KB file not found at " ++ fp
  process (_, kb_fp) = do
    lkb <- loadKb kb_fp
    let k = pure $ pure a
    let s = (\k' -> "Removed: " ++ show k') <$> k
    e <- unwrapIOError (remove <$> lkb <*> k)
    return $ (,) <$> s <*> e


-- | List command handler.
-- Displays all the contents of the specified Knowledge base.
listHandler
  :: Maybe FilePath -- ^ KB file path
  -> IO () -- ^ Writes the full contents of the KB to the STDOUT.
listHandler kb_mfp = do
  kb_fp     <- unwrapIOError $ getKnowledgeBaseFilePath kb_mfp
  kb_exists <- doesFileExist kb_fp
  res       <- process (kb_exists, kb_fp)
  unwrapIOError res >>= putStrLn
 where
  process :: (Bool, FilePath) -> IO (Either Error String)
  process (False, fp) = do
    return $ Left $ StandardError $ "KB file not found at " ++ fp
  process (_, kb_fp) = do
    lkb <- loadKb kb_fp
    let rs = map formatRecord . listAll <$> lkb
    return $ intercalate "\n" <$> rs
