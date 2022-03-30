module Main where

import           Test.DocTest                   ( doctest )

-- |Code locations included in the doctest suite.
doctestCoverage :: [[Char]]
doctestCoverage = ["lib-cli", "lib-core"]

-- |Configuration of the application doctest's.
--
-- See: <https://hackage.haskell.org/package/doctest>
main :: IO ()
main = doctest doctestCoverage
