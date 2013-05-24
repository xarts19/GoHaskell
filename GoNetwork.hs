module GoNetwork
( startServer
, startClient
) where


import Game
import Network
import System.IO
import Data.Lens.Lazy ( (^%=), (^.) )
import Control.Monad ( unless )


commPort :: PortID
commPort = PortNumber 19199


startServer :: GameOptions -> IO ()
startServer opts = withSocketsDo $ do
    sock <- listenOn commPort
    serverHandler opts sock


startClient :: HostName -> IO ()
startClient server = withSocketsDo $ do
    handle <- connectTo server commPort
    hSetBuffering handle NoBuffering

    -- recieve game options from server
    optsLine <- hGetLine handle
    let optsRaw = read optsLine :: GameOptions

    -- change current player as recieved options have other player there
    let opts = playerColor ^%= pOpposite $ optsRaw
    putStrLn $ "Game options: " ++ show opts

    gameHandler handle opts


serverHandler :: GameOptions -> Socket -> IO ()
serverHandler opts sock = do
    putStrLn "Waiting for client to connect..."
    (handle, hostName, _) <- accept sock
    hSetBuffering handle NoBuffering
    putStrLn $ "New client: " ++ hostName

    -- send game options to client
    hPrint handle opts
    putStrLn $ "Game options: " ++ show opts

    -- start game
    gameHandler handle opts


gameHandler :: Handle -> GameOptions -> IO ()
gameHandler handle opts = do
    st <- initGame opts
    let player = opts^.playerColor
    let local = player == PBlack
    handleTurns st handle local


handleTurns :: GameState -> Handle -> Bool -> IO ()
handleTurns st handle local = do
    showStats st
    move <- if local then readMoveLocal handle st else readMoveRemote handle
    newSt <- makeMove st move
    unless (newSt^.gameOver) $ handleTurns newSt handle $ not local


readMoveRemote :: Handle -> IO Move
readMoveRemote handle = do
    putStrLn "Wating for opponent's move..."
    move <- fmap read $ hGetLine handle
    putStrLn $ "Opponent's move: " ++ showMove move
    return move


readMoveLocal :: Handle -> GameState -> IO Move
readMoveLocal handle st = do
    putStrLn "Your move:"
    move <- readMove st
    hPrint handle move
    return move


