module Main where

import           Test.DocTest (doctest)

-- |Code locations included in the doctest suite.
doctestCoverage :: [String]
doctestCoverage = ["lib-cli", "lib-core"]

-- |Doctest options for the doctest CLI execution.
doctestOptions :: [String]
doctestOptions = []

-- |Configuration of the application doctest's.
--
-- See: <https://hackage.haskell.org/package/doctest>
main :: IO ()
main = doctest $ doctestOptions ++ doctestCoverage
