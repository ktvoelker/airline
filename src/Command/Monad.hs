
module Command.Monad
  ( CM
  , runCM
  , log
  , setPaused
  , setSpeed
  , atomically
  , newStdGen
  , getGame
  , module Control.Concurrent.STM
  , module System.Random
  ) where

import Control.Concurrent.STM hiding (atomically)
import qualified Control.Concurrent.STM as STM
import Control.Monad.Operational
import Data.IORef
import Data.Time.Clock
import H.IO
import H.Prelude
import System.Random hiding (getStdRandom, getStdGen, setStdGen, newStdGen, randomRIO, randomIO)
import qualified System.Random as R

import Simulation
import Types (Game())

data CommandInstruction a where
  CILog        :: Text -> CommandInstruction ()
  CISetPaused  :: Bool -> CommandInstruction ()
  CISetSpeed   :: NominalDiffTime -> CommandInstruction ()
  CIAtomically :: STM a -> CommandInstruction a
  CINewStdGen  :: CommandInstruction StdGen
  CIGetGame    :: CommandInstruction Game

type CM = Program CommandInstruction

runCM :: CM a -> MasterHandle g p -> Game -> IO a
runCM m mh game = interpretWithMonad (interpreter mh game) m

interpreter :: MasterHandle g p -> Game -> CommandInstruction a -> IO a
interpreter mh game = \case
  CILog xs -> putStrLn xs
  CISetPaused paused -> writeIORef (mhPaused mh) paused
  CISetSpeed speed -> writeIORef (mhSpeed mh) speed
  CIAtomically stm -> STM.atomically stm
  CINewStdGen -> R.newStdGen
  CIGetGame -> pure game

log :: Text -> CM ()
log = singleton . CILog

setPaused :: Bool -> CM ()
setPaused = singleton . CISetPaused

setSpeed :: NominalDiffTime -> CM ()
setSpeed = singleton . CISetSpeed

atomically :: STM a -> CM a
atomically = singleton . CIAtomically

newStdGen :: CM StdGen
newStdGen = singleton CINewStdGen

getGame :: CM Game
getGame = singleton CIGetGame

