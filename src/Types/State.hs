
module Types.State where

import H.Common

newtype SimT e s m a = SimT { unSimT :: StateT s (ReaderT e m) a }

-- TODO Functor/Applicative/Monad for SimT

