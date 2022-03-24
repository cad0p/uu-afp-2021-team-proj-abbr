module Main where

import           LibCore.Decoder
import           LibCore.InputInterface
import           LibCore.KnowledgeBase
import           LibCore.Mapper
import           LibCore.Matcher
import           LibCore.OutputInterface
import           LibCore.Parser

main :: IO ()
main = returnOutput $ decode $ mapParseStructure k $ match k $ parse getInput
  where
    k = getKnowledgeBase
