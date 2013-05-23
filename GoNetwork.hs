module GoNetwork
( startServer
, startClient
) where


import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)


startServer :: IO ()
startServer = withSocketsDo $ do
    sock <- listenOn $ PortNumber 19199
    sockHandler sock


sockHandler :: Socket -> IO ()
sockHandler sock = do
    (handle, hostName, _) <- accept sock
    putStrLn $ "New client: " ++ hostName
    hSetBuffering handle NoBuffering
    forkIO $ commandProcessor handle
    readLocalInput


commandProcessor :: Handle -> IO ()
commandProcessor handle = do
    line <- hGetLine handle
    let cmd = words line
    case head cmd of
        _ -> hPutStrLn handle "Unknown command"
    commandProcessor handle






