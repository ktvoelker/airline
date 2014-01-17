
module Main where

import H.Common

import Server

handler :: Handler
handler = writeMessage

main :: IO ()
main = do
  tid <- run handler
  todo tid

