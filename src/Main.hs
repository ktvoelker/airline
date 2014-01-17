
module Main where

import H.Common

import Server

main :: IO ()
main = do
  tid <- run
  todo tid

