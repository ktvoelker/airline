
module Main where

import H.Common

import WebSockets

main :: IO ()
main = do
  tid <- run
  todo tid

