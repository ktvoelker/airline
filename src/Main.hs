
module Main where

import Control.Concurrent.Future
import H.Common

import Server

handler :: Handler
handler server cid msg = do
  print msg
  writeMessage server cid msg

main :: IO ()
main = void $ forkPromise (run handler) >>= wait

