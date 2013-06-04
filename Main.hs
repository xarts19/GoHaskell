import Gui
import Game
import GoNetwork ( startServer, startClient )
import Board ( Cell(..), opposite )
import System.Environment
import Data.Char ( toLower )
import Data.Lens.Lazy ( (^=), (^.) )
import Control.Monad ( unless )


getBoardSize :: IO Int
getBoardSize = do putStrLn "Choose board size: s - Small (9), m - Medium (13), l - Large (19):"
                  choice <- getLine
                  case map toLower choice of
                     s | s `elem` ["small",  "s", "9"]  -> return 9
                       | s `elem` ["medium", "m", "13"] -> return 13
                       | s `elem` ["large",  "l", "19"] -> return 19
                       | otherwise                      -> do putStrLn "Wrong! Try again"
                                                              getBoardSize


getColor :: IO Player
getColor = do putStrLn "Choose your side: b - Black, w - White"
              choice <- getLine
              case map toLower choice of
                   s | s `elem` ["b", "black"] -> return PBlack
                     | s `elem` ["w", "white"] -> return PWhite
                     | otherwise               -> do putStrLn "Wrong! Try again"
                                                     getColor


menuLoop :: GameOptions -> [String] -> IO ()
menuLoop options args = do
    putStrLn $ "Options: " ++ showOptions options
    putStrLn "1. Single player"
    putStrLn "2. Multiplayer"
    putStrLn "3. Host LAN game"
    putStrLn "4. Join LAN game"
    putStrLn "5. Set board size"
    putStrLn "6. Set your side"
    putStrLn "7. Exit"
    choice <- getLine
    case choice of
         "2"       -> do startMultiplayer options
                         menuLoop options args

         "3"       -> do startServer options
                         menuLoop options args

         "4"       -> do putStrLn "Enter server ip/hostname (default: localhost):"
                         host <- getLine
                         startClient $ if host == "" then "localhost" else host
                         menuLoop options args

         "5"       -> do newBoardSize <- getBoardSize
                         let newOptions = boardSize ^= newBoardSize $ options
                         menuLoop newOptions args

         "6"       -> do newColor <- getColor
                         let newOptions = playerColor ^= newColor $ options
                         menuLoop newOptions args

         "7"       -> return ()

         _         -> menuLoop options args


startMultiplayer :: GameOptions -> IO ()
startMultiplayer opts = do
    st <- initGame opts
    handleTurns st Black


handleTurns :: GameState -> Cell -> IO ()
handleTurns st color = do
    showStats st
    putStrLn $ show color ++ " moves: "
    move <- readMove st
    newSt <- makeMove st move
    unless (newSt^.gameOver) $ handleTurns newSt $ opposite color


main :: IO ()
main = do
    args <- getArgs
    if "--console" `elem` args then menuLoop defaultOptions args
                               else mainWithGui defaultOptions args

