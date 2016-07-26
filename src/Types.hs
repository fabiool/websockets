module Types ( Protocol(..)
             , Config(..)
             , Connection
             , ConnectionsCount
             , DelayInMicrosecs
             , DispatchQueue
             , ShutdownControlVar
             , ThreadsCount
             ) where



import Control.Concurrent.STM.TVar ( TVar(..) )
import Control.Concurrent.STM.TChan ( TChan(..) )

import Config (Config(..))

type Connection = String
type ConnectionsCount = Int
type DelayInMicrosecs = Int
type DispatchQueue = TChan String
type ShutdownControlVar = TVar Bool
type ThreadsCount = Int

data Protocol = Protocol { connectionAcceptor  :: DispatchQueue -> IO ()
                         , connectionHandler   :: Connection -> IO ()
                         , maxConnections      :: ConnectionsCount
                         , freeSlotWaitingTime :: DelayInMicrosecs }

