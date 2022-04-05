{-|
Description : TODO: Outputs stuff
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCli.OutputInterface where

returnOutput :: Maybe FilePath -> String -> IO ()
returnOutput f = case f of
  Nothing -> error "No output file found"
  Just s  -> writeFile s
