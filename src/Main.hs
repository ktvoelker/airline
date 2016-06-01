
module Main where

import H.IO

import CLI
import Command.Simple (speedToCycleLength, SimulationSpeed(SpeedSlow))
import Demo
import Game
import Simulation

main :: IO ()
main = do
  game <- demo
  masterHandle <- forkSim (speedToCycleLength SpeedSlow) gameSim game
  runCLI masterHandle game

