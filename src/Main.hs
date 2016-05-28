
module Main where

import H.IO
import H.Prelude

import CLI
import Game
import Simulation
import Types.Command

main :: IO ()
main = do
  masterHandle <- forkSim (speedToCycleLength SpeedSlow) gameSim todo
  runCLI masterHandle

