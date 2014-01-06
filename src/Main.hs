
module Main where

import H.Common

import View

main :: IO ()
main = startView >>= waitForView

