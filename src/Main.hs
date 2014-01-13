
module Main where

import H.Chan
import H.Common

import qualified Server

main :: IO ()
main = do
  (rChan, wChan) <- newSplitChan
  server <- Server.run wChan
  todo server rChan

