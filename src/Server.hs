
module Server ( runServer ) where

import Control.Concurrent ( threadDelay )
import Control.Concurrent.STM ( atomically )
import Control.Concurrent.STM.TChan ( TChan(..), newTChanIO, readTChan )
import Control.Concurrent.STM.TVar ( TVar(..), readTVarIO, writeTVar )
import Control.Concurrent.Thread.Group ( ThreadGroup )
import Control.Exception ( bracket )
import Control.Monad ( when, unless )
import Data.Time.Clock ( getCurrentTime )
import Data.Time.Format ( formatTime, defaultTimeLocale, iso8601DateFormat )
import System.IO ( FilePath, IOMode(WriteMode), Handle, openFile, hFlush, hClose, hPutStrLn, stdin, stdout )
import qualified Control.Concurrent.Thread.Group as ThreadGroup ( new, forkIO, waitN, nrOfRunning )
import qualified Data.UUID as U ( toString )
import qualified Data.UUID.V4 as V4 ( nextRandom )

import Types

runServer :: Config -> Protocol -> ShutdownControlVar -> IO ()
runServer c p v =
    do

        runConditionally runServer'

    where

        forkIO' :: ThreadGroup -> IO () -> IO ()
        forkIO' t a = do
            ThreadGroup.forkIO t a
            return ()

        runServer' :: IO ()
        runServer' = do

            sid' <- V4.nextRandom

            let sid = U.toString sid'

            t <- ThreadGroup.new

            q <- newTChanIO :: IO (TChan String)

            f <- threadsFile sid

            bracket (openFile f WriteMode) hClose $ \h -> do

                let n = maxConnections p

                forkIO' t (commandPrompt t) -- TODO: admin prompt should move to main thread.
                forkIO' t (threadsCounter h t)
                forkIO' t (connectionListener n t q)
                forkIO' t (connectionDispatcher n t q)

                ThreadGroup.waitN 2 t

            return ()

        commandPrompt :: ThreadGroup -> IO ()
        commandPrompt t = commandPrompt'
            where
                commandPrompt' :: IO ()
                commandPrompt' = do
                    putStr $ "(serverAdmin) : "
                    hFlush stdout
                    cmd <- getLine
                    dispatchCommand cmd
                    commandPrompt t

                dispatchCommand :: String -> IO ()
                dispatchCommand "shutdown" = do
                    atomically (writeTVar v True)
                    putStrLn $ "Server will terminate"
                dispatchCommand cmd = do
                    putStrLn $ "Invalid command!"

        threadsCounter :: Handle -> ThreadGroup -> IO ()
        threadsCounter h t = runConditionally threadsCounter'
            where
                threadsCounter' = do
                    ts <- timeStamp
                    tc <- fetchRunningThreadsCount t
                    hPutStrLn h $ unwords ["Thread count at", ts, ":", (show tc), "threads."]
                    hFlush h
                    threadDelay $ counterDelay c
                    threadsCounter h t

        connectionListener :: ConnectionsCount -> ThreadGroup -> DispatchQueue -> IO ()
        connectionListener n t q = runConditionally connectionListener'
            where
                connectionListener' = do
                    currentThreads <- fetchRunningThreadsCount t
                    if (currentThreads >= maxThreads n) then
                        waitForAFreeSlot
                    else
                        acceptConnection q
                    connectionListener n t q

        connectionDispatcher :: ConnectionsCount -> ThreadGroup -> DispatchQueue -> IO ()
        connectionDispatcher n t q = runConditionally connectionDispatcher'
            where
                connectionDispatcher' = do
                    currentThreads <- fetchRunningThreadsCount t
                    if (currentThreads >= maxThreads n) then
                        waitForAFreeSlot
                    else do
                        x <- atomically (readTChan q)
                        forkIO' t (handleConnection x)
                        return ()
                    connectionDispatcher n t q

        logDir :: FilePath
        logDir = case trace c of
            Nothing  -> "."
            Just dir -> dir

        threadsFile :: String -> IO FilePath
        threadsFile sid = do
            ts <- timeStamp
            return $ logDir ++ "/" ++ ts ++ "_" ++ sid ++ "-threads-count.txt"

        timeStamp :: IO String
        timeStamp = do
            now <- getCurrentTime
            return $ formatTime defaultTimeLocale formatSpec now

        formatSpec :: String
        formatSpec = iso8601DateFormat (Just "%H:%M:%S")

        waitForAFreeSlot :: IO ()
        waitForAFreeSlot = threadDelay $ freeSlotWaitingTime p

        fetchRunningThreadsCount :: ThreadGroup -> IO Int
        fetchRunningThreadsCount t = atomically (ThreadGroup.nrOfRunning t)

        acceptConnection :: DispatchQueue -> IO ()
        acceptConnection q = connectionAcceptor' q
            where
                connectionAcceptor' = connectionAcceptor p

        handleConnection :: Connection -> IO ()
        handleConnection c = connectionHandler' c
            where
                connectionHandler' = connectionHandler p

        runConditionally :: IO () -> IO ()
        runConditionally a = do
            b <- readTVarIO v
            unless b a

serverThreads :: ThreadsCount
serverThreads = 4

--counterDelay :: DelayInMicrosecs
--counterDelay = 1000000

maxThreads :: ConnectionsCount -> ThreadsCount
maxThreads maxConn = maxConn + serverThreads
