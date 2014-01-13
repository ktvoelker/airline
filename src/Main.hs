
module Main where

import Control.Concurrent
import H.Common

import qualified Server

main :: IO ()
main = do
  chan <- newChan
  server <- Server.run (writeChan chan)
  todo server

