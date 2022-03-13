module Main where

import Decoder
import InputInterface
import KnowledgeBase
import Mapper
import Matcher
import OutputInterface
import Parser

main :: IO ()
main = returnOutput $ decode $ mapParseStructure k $ match k $ parse getInput
  where
    k = getKnowledgeBase
