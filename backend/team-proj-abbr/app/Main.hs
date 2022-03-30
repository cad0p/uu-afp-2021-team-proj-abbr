module Main where
import qualified LibCli.CliMain as CM (cliMain)


-- |Basic CLI executable entrypoint
main :: IO ()
main = CM.cliMain
