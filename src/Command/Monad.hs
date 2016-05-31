
module Command.Monad
  ( CSTM()
  , CM()
  , runCM
  , log
  , setPaused
  , setSpeed
  , atomically
  , newStdGen
  , getGame
  , module Control.Concurrent.STM
  , module Control.Monad.STM.Class
  , module System.Random
  ) where

import Control.Concurrent.STM hiding (atomically)
import qualified Control.Concurrent.STM as STM
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

newtype CSTM e a = CSTM { unCSTM :: ReaderT Game (ExceptT e STM) a }
  deriving (Functor, Applicative, Monad, MonadReader Game, MonadError e, MonadSTM)

data CommandInstruction e a where
  CILog        :: Text -> CommandInstruction e ()
  CISetPaused  :: Bool -> CommandInstruction e ()
  CISetSpeed   :: NominalDiffTime -> CommandInstruction e ()
  CIAtomically :: CSTM e a -> CommandInstruction e a
  CINewStdGen  :: CommandInstruction e StdGen
  CIGetGame    :: CommandInstruction e Game
  CICatchError :: CM e a -> (e -> CM e a) -> CommandInstruction e a
  CIThrowError :: e -> CommandInstruction e a

newtype CM e a = CM { unCM :: Program (CommandInstruction e) a }
  deriving (Functor, Applicative, Monad)

instance MonadError e (CM e) where
  catchError m f = CM . singleton $ CICatchError m f
  throwError = CM . singleton . CIThrowError

runCM :: CM e a -> MasterHandle g p -> Game -> IO (Either e a)
runCM m mh game = case view $ unCM m of
  Return x -> pure $ Right x
  m' :>>= k -> case m' of
    CILog xs -> putStrLn xs >>= continue k
    CISetPaused paused -> writeIORef (mhPaused mh) paused >>= continue k
    CISetSpeed speed -> writeIORef (mhSpeed mh) speed >>= continue k
    CIAtomically cstm -> STM.atomically (runExceptT $ runReaderT (unCSTM cstm) game) >>= \case
      Left err -> pure $ Left err
      Right b -> continue k b
    CINewStdGen -> R.newStdGen >>= continue k
    CIGetGame -> continue k game
    CICatchError m'' f -> runCM m'' mh game >>= \case
      Left err -> runCM (f err >>= CM . k) mh game
      Right b -> continue k b
    CIThrowError err -> pure $ Left err
  where
    continue :: (a -> Program (CommandInstruction e) b) -> a -> IO (Either e b)
    continue k b = runCM (CM $ k b) mh game

log :: Text -> CM e ()
log = CM . singleton . CILog

setPaused :: Bool -> CM e ()
setPaused = CM . singleton . CISetPaused

setSpeed :: NominalDiffTime -> CM e ()
setSpeed = CM . singleton . CISetSpeed

atomically :: CSTM e a -> CM e a
atomically = CM . singleton . CIAtomically

newStdGen :: CM e StdGen
newStdGen = CM $ singleton CINewStdGen

getGame :: CM e Game
getGame = CM $ singleton CIGetGame

