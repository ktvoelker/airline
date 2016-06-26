
module Command.Monad
  ( MonadCommand(..)
  , CSTM
  , runCSTM
  , throwSTM
  , CM
  , runCM
  , log
  , setPaused
  , setSpeed
  , atomically
  , newStdGen
  , module Control.Concurrent.STM
  , module Control.Monad.STM.Class
  , module System.Random
  ) where

import Control.Concurrent.STM hiding (atomically, throwSTM)
import qualified Control.Concurrent.STM as STM
import Control.Exception
import Control.Monad.Operational
import Control.Monad.STM.Class
import Data.IORef
import Data.Time.Clock
import H.IO
import H.Prelude
import System.Random hiding (getStdRandom, getStdGen, setStdGen, newStdGen, randomRIO, randomIO)
import qualified System.Random as R

import Simulation
import Types (Game())

class (Monad m) => MonadCommand m where
  getGame :: m Game

data CommandSTMInstruction a where
  CSLift    :: STM a -> CommandSTMInstruction a
  CSThrow   :: (Exception e) => e -> CommandSTMInstruction a
  CSGetGame :: CommandSTMInstruction Game

newtype CSTM a = CSTM { unCSTM :: Program CommandSTMInstruction a }
  deriving (Functor, Applicative, Monad)

instance MonadCommand CSTM where
  getGame = CSTM $ singleton CSGetGame

instance MonadSTM CSTM where
  liftSTM = CSTM . singleton . CSLift

runCSTM :: CSTM a -> Game -> STM a
runCSTM m game = case view $ unCSTM m of
  Return x -> pure x
  m' :>>= k -> case m' of
    CSLift stm -> stm >>= continue k
    CSThrow e  -> STM.throwSTM e >>= continue k
    CSGetGame  -> continue k game
  where
    continue :: (a -> Program CommandSTMInstruction b) -> a -> STM b
    continue k b = runCSTM (CSTM $ k b) game

throwSTM :: (Exception e) => e -> CSTM a
throwSTM = CSTM . singleton . CSThrow

data CommandInstruction a where
  CILog        :: Text -> CommandInstruction ()
  CISetPaused  :: Bool -> CommandInstruction ()
  CISetSpeed   :: NominalDiffTime -> CommandInstruction ()
  CIAtomically :: CSTM a -> CommandInstruction a
  CINewStdGen  :: CommandInstruction StdGen
  CIGetGame    :: CommandInstruction Game

type CM = Program CommandInstruction

instance MonadCommand CM where
  getGame = singleton CIGetGame

runCM :: CM a -> MasterHandle -> Game -> IO a
runCM m mh game = case view m of
  Return x -> pure x
  m' :>>= k -> case m' of
    CILog xs -> putStrLn xs >>= continue k
    CISetPaused paused -> writeIORef (mhPaused mh) paused >>= continue k
    CISetSpeed speed -> writeIORef (mhSpeed mh) speed >>= continue k
    CIAtomically cstm -> STM.atomically (runCSTM cstm game) >>= continue k
    CINewStdGen -> R.newStdGen >>= continue k
    CIGetGame -> continue k game
  where
    continue :: (a -> Program CommandInstruction b) -> a -> IO b
    continue k b = runCM (k b) mh game

log :: Text -> CM ()
log = singleton . CILog

setPaused :: Bool -> CM ()
setPaused = singleton . CISetPaused

setSpeed :: NominalDiffTime -> CM ()
setSpeed = singleton . CISetSpeed

atomically :: CSTM a -> CM a
atomically = singleton . CIAtomically

newStdGen :: CM StdGen
newStdGen = singleton CINewStdGen

