module Main where
import qualified LibCli.Main as CM (cliMain)


-- |Basic CLI executable entrypoint
main :: IO ()
main = CM.cliMain
