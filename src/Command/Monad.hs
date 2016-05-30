
module Command.Monad
  ( CM
  , runCM
  , log
  , setPaused
  , setSpeed
  , atomically
  , newStdGen
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

data CommandInstruction a where
  CILog        :: Text -> CommandInstruction ()
  CISetPaused  :: Bool -> CommandInstruction ()
  CISetSpeed   :: NominalDiffTime -> CommandInstruction ()
  CIAtomically :: STM a -> CommandInstruction a
  CINewStdGen  :: CommandInstruction StdGen

type CM a = Program CommandInstruction a

runCM :: CM a -> MasterHandle g p () () -> IO a
runCM m mh = interpretWithMonad (interpreter mh) m

interpreter :: MasterHandle g p () () -> CommandInstruction a -> IO a
interpreter mh = \case
  CILog xs -> putStrLn xs
  CISetPaused paused -> writeIORef (mhPaused mh) paused
  CISetSpeed speed -> writeIORef (mhSpeed mh) speed
  CIAtomically stm -> STM.atomically stm
  CINewStdGen -> R.newStdGen

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

