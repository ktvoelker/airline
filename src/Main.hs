
module Main where

import H.IO
import H.Prelude

import CLI
import Demo
import Game
import Simulation
import Types.Command

main :: IO ()
main = do
  masterHandle <- demo >>= forkSim (speedToCycleLength SpeedSlow) gameSim
  runCLI masterHandle

