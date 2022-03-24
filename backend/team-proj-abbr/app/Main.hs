module Main where

import           LibCore.Decoder         (decode)
import           LibCore.InputInterface  (getInput)
import           LibCore.KnowledgeBase   (getKnowledgeBase)
import           LibCore.Mapper          (mapParseStructure)
import           LibCore.OutputInterface (returnOutput)
import           LibCore.Parser          (parseInput)

main :: IO ()
main = returnOutput $ decode $ mapParseStructure getKnowledgeBase $ parseInput getInput
