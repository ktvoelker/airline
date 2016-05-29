
module Main where

import H.IO

import CLI
import Demo
import Game
import Simulation
import Types.Command

main :: IO ()
main = do
  game <- demo
  masterHandle <- forkSim (speedToCycleLength SpeedSlow) gameSim game
  runCLI masterHandle game

