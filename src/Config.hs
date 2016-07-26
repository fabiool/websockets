module Config ( Config(..), parseArguments ) where

import Control.Monad ( when )
import Control.Concurrent.STM.TMVar ( newTMVarIO )
import Data.List ( findIndex, findIndices )
import Data.Maybe ( fromMaybe )
import System.Console.GetOpt
import System.Environment ( getProgName )
import System.Exit ( exitFailure )
import Network.Socket (HostName, ServiceName)

data Config = Config { verbose          :: Bool
                     , client           :: Bool
                     , host             :: HostName
                     , port             :: ServiceName
                     , trace            :: Maybe FilePath
                     , text             :: Maybe FilePath
                     , binary           :: Maybe FilePath
                     , instNum          :: Int
                     , maxConn          :: Int
                     , counterDelay     :: Int
                     , waitTime         :: Int
                     , delayFactor      :: Int }
            deriving (Show)

data Options = Help
             | Verbose
             | Client
             | Host String
             | Port String
             | Trace String
             | Text String
             | Binary String
             | InstNum String
             | MaxConn String
             | CounterDelay String
             | WaitTime String
             | DelayFactor String
             deriving (Eq,Show)

options :: [OptDescr Options]
options =
  [ Option ['h','?']  ["help"]             (NoArg Help)                 "Show help"
  , Option ['v']      ["verbose"]          (NoArg Verbose)              "Debug output on stderr"
  , Option ['c']      ["client"]           (NoArg Client)               "Starts in client mode"
  , Option ['n']      ["host"]             (ReqArg Host "HOST")         "HOST name to connect or bind to"
  , Option ['p']      ["port"]             (ReqArg Port "PORT")         "PORT number to connect of bind to"
  , Option ['t']      ["trace-dir"]        (ReqArg Trace "DIR")         "Directory where to save received packets"
  , Option ['x']      ["text"]             (ReqArg Text "FILE")         "Text content to be sent to the other party"
  , Option ['b']      ["binary"]           (ReqArg Binary "FILE")       "Binary content to be sent to the other party"
  , Option ['i']      ["instances"]        (ReqArg InstNum "NUM")     "Number of concurrent instances to launch"
  , Option ['m']      ["max-connections"]  (ReqArg MaxConn "NUM")      "Maximun number of connections per server instance"
  , Option ['d']      ["counter-delay"]    (ReqArg CounterDelay "NUM")  "Polls the thread group every 'countDelay' microsecs"
  , Option ['w']      ["wait-time"]        (ReqArg WaitTime "NUM")      "Wait time until trying againg for next available connection slot"
  , Option ['f']      ["delay-factor"]     (ReqArg DelayFactor "NUM")   "Delay factor"
  ]

type ParsedOptions = ([Options],[String],[String])

parseArguments :: [String] -> IO Config
parseArguments argv =
    do
        let r@(o,u,errs) = getOpt Permute options argv

        when (parsingFailed        r)  (abort (concat errs))
        when (unknownOptions       u)  (abort "Unknown options given")
        when (askedHelp            r)  (abort "")
        when (missingHostname      r)  (abort "Hostname is required")
        when (multipleHostnames    r)  (abort "Only one hostname must be informed")
        when (missingServicename   r)  (abort "Port number is required")
        when (multipleServicenames r)  (abort "Only one port number must be informed")

        return Config { verbose         = (elem Verbose o)
                      , client          = (elem Client o)
                      , host            = ((\(Host x) -> x) $ head $ filter isHost o)
                      , port            = ((\(Port y) -> y) $ head $ filter isPort o)
                      , trace           = trace' o Nothing
                      , text            = text' o Nothing
                      , binary          = binary' o Nothing
                      , instNum         = instNum' o 1
                      , maxConn         = maxConn' o 5
                      , counterDelay    = counterDelay' o 1000000
                      , waitTime        = waitTime' o 5000000
                      , delayFactor     = delayFactor' o 1 }

    where




        unknownOptions :: [String] -> Bool
        unknownOptions x = case x of [] -> False
                                     _  -> True




        abort :: String -> IO ()
        abort m = (printHelp m >> exitFailure >> return ())





        printHelp :: String -> IO ()
        printHelp m =
            do
                putStrLn $ filter (\c -> c /= '\n') m
                progName <- getProgName
                let header = "Usage: " ++ progName ++ " " ++ optionsStr
                putStrLn $ usageInfo header options
            where
                optionsStr = unwords $ [ "[-h|-?|--help]"
                                       , "[-v|--verbose]"
                                       , "[-c|--client]"
                                       , "-n[HOST]|--hostname=HOST"
                                       , "-p[PORT]|--hostname=PORT"
                                       , "\n"
                                       , (take 20 $ repeat ' ')
                                       , "[-t[DIR]|--trace-dir=DIR]"
                                       , "[-x[FILE]|--text=FILE]"
                                       , "[-b[FILE]|--binary=FILE]"
                                       , "[-i[NUM]|--instances=NUM]"
                                       , "\n"
                                       , (take 20 $ repeat ' ')
                                       , "[-m[NUM]|--max-connections=NUM]"
                                       , "[-d[NUM]|--counter-delay=NUM]"
                                       , "[-w[NUM]|--wait-time=NUM]"
                                       , "\n"
                                       , (take 20 $ repeat ' ')
                                       , "[-f[NUM]|--delay-factor=NUM]"
                                       , "\n" ]





        askedHelp :: ParsedOptions -> Bool
        askedHelp (o,_,_) = elem Help o





        parsingFailed :: ParsedOptions -> Bool
        parsingFailed (_,_,errs) = errs /= []





        missingHostname :: ParsedOptions -> Bool
        missingHostname (o,_,[]) = case findIndex isHost o of Just _ -> False
                                                              _      -> True




        multipleHostnames :: ParsedOptions -> Bool
        multipleHostnames (o,_,[]) = length (findIndices isHost o) > 1





        missingServicename :: ParsedOptions -> Bool
        missingServicename (o,_,[]) = case findIndex isPort o of Just _ -> False
                                                                 _      -> True




        multipleServicenames :: ParsedOptions -> Bool
        multipleServicenames (o,_,[]) = length (findIndices isPort o) > 1





        isHost :: Options -> Bool
        isHost x = case x of (Host _) -> True
                             _        -> False





        isPort :: Options -> Bool
        isPort x = case x of (Port _) -> True
                             _        -> False





        isTrace :: Options -> Bool
        isTrace x = case x of (Trace _) -> True
                              _         -> False





        isText :: Options -> Bool
        isText x = case x of (Text _) -> True
                             _        -> False





        isBinary :: Options -> Bool
        isBinary x = case x of (Binary _) -> True
                               _          -> False





        isInstNum :: Options -> Bool
        isInstNum x = case x of (InstNum _) -> True
                                _           -> False





        isMaxConn :: Options -> Bool
        isMaxConn x = case x of (MaxConn _) -> True
                                _           -> False





        isCounterDelay :: Options -> Bool
        isCounterDelay x = case x of (CounterDelay _) -> True
                                     _                -> False





        isWaitTime :: Options -> Bool
        isWaitTime x = case x of (WaitTime _) -> True
                                 _            -> False





        isDelayFactor :: Options -> Bool
        isDelayFactor x = case x of (DelayFactor _) -> True
                                    _               -> False




        safeHead :: [Options] -> Maybe Options
        safeHead [] = Nothing
        safeHead xs = Just $ head xs





        instNum' :: [Options] -> Int -> Int
        instNum' o v = parse $ safeHead $ filter isInstNum o
            where
                parse Nothing = v
                parse (Just (InstNum y)) = read y :: Int






        maxConn' :: [Options] -> Int -> Int
        maxConn' o v = parse $ safeHead $ filter isMaxConn o
            where
                parse Nothing = v
                parse (Just (MaxConn y)) = read y :: Int






        counterDelay' :: [Options] -> Int -> Int
        counterDelay' o v = parse $ safeHead $ filter isCounterDelay o
            where
                parse Nothing = v
                parse (Just (CounterDelay y)) = read y :: Int






        waitTime' :: [Options] -> Int -> Int
        waitTime' o v = parse $ safeHead $ filter isWaitTime o
            where
                parse Nothing = v
                parse (Just (WaitTime y)) = read y :: Int






        delayFactor' :: [Options] -> Int -> Int
        delayFactor' o v = parse $ safeHead $ filter isDelayFactor o
            where
                parse Nothing = v
                parse (Just (DelayFactor y)) = read y :: Int






        trace' :: [Options] -> Maybe FilePath -> Maybe FilePath
        trace' o v = parse $ safeHead $ filter isTrace o
            where
                parse Nothing = v
                parse (Just (Trace y)) = Just y






        text' :: [Options] -> Maybe FilePath -> Maybe FilePath
        text' o v = parse $ safeHead $ filter isText o
            where
                parse Nothing = v
                parse (Just (Text y)) = Just y






        binary' :: [Options] -> Maybe FilePath -> Maybe FilePath
        binary' o v = parse $ safeHead $ filter isBinary o
            where
                parse Nothing = v
                parse (Just (Binary y)) = Just y


