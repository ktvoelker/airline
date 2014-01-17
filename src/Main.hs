
module Main where

import Control.Concurrent.Future
import H.Common

import Server

handler :: Handler
handler = writeMessage

main :: IO ()
main = void $ forkPromise (run handler) >>= wait

