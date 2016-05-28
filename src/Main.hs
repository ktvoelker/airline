
module Main where

import H.IO
import H.Prelude

import CLI
import Game
import Simulation
import Types ()

main :: IO ()
main = do
  masterHandle <- forkSim gameSim todo
  runCLI masterHandle

