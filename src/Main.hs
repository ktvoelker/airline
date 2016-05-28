
module Main where

import H.IO

import CLI
import Demo
import Game
import Simulation
import Types.Command

main :: IO ()
main = do
  masterHandle <- forkSim (speedToCycleLength SpeedSlow) gameSim demo
  runCLI masterHandle

