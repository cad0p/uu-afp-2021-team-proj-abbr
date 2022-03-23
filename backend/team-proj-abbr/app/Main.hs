module Main where

import           Decoder
import           InputInterface
import           KnowledgeBase
import           Mapper
import           OutputInterface
import           Parser

main :: IO ()
main = returnOutput $ decode $ mapParseStructure getKnowledgeBase $ parseInput getInput
