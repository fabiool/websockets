
module Main ( main ) where

import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( async, wait )
import Control.Concurrent.STM ( atomically )
import Control.Concurrent.STM.TChan ( writeTChan )
import Control.Concurrent.STM.TVar ( newTVarIO )
import System.Environment ( getArgs )
import System.Random ( getStdRandom, randomR )
import qualified Data.UUID as U ( toString )
import qualified Data.UUID.V4 as V4 ( nextRandom )

import Config
import Types
import Server

main :: IO ()
main =
    do
        c <- (parseArguments =<< getArgs)

        let p = Protocol { connectionAcceptor = acceptor
                         , connectionHandler = handler
                         , maxConnections = maxConn c
                         , freeSlotWaitingTime = waitTime c }

        v <- newTVarIO False

        let serverAction = runServer c p v

        (mapM async (take (instNum c) $ repeat serverAction)) >>= (mapM wait)

        return ()

    where

        acceptor :: DispatchQueue -> IO ()
        acceptor q = do
            secs <- rollDice
            threadDelay (secs)
            c <- nextUUID
            atomically (writeTChan q c)

        handler :: Connection -> IO ()
        handler c = do
            secs <- rollDice
            threadDelay (secs * 1000000)

        nextUUID :: IO String
        nextUUID = do
            i <- V4.nextRandom
            return (U.toString i)

        rollDice :: IO Int
        rollDice = getStdRandom (randomR (1,6))
