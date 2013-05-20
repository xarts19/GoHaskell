{-# LANGUAGE TemplateHaskell #-}

import Board
import Rules
import System.Environment
import Data.Char ( toLower )
import Data.Lens.Lazy ( (^=), (^.) )
import Data.Lens.Template ( makeLenses )
import Control.Monad ( unless )

data GameOptions = GameOptions
                    { _boardSize :: Int
                    , _playerColor :: Cell
                    , _difficulty :: Int
                    , _komi :: Float }

data GameState = GameState
                  { _curBoard :: Board.Board
                  , _score :: Float  -- Black : positive, White : negative
                  , _whoseTurn :: Cell
                  , _passes :: Int
                  , _prevBoard :: Board.Board }

$( makeLenses [''GameOptions,''GameState] )

instance Show GameOptions where
    show options = concat ["Size - ", show $ options^.boardSize, "; ",
                           "Player color - ", show $ options^.playerColor, "; ",
                           "Difficulty - ", show $ options^.difficulty, "; ",
                           "Komi - ", show $ options^.komi]

defaultOptions :: GameOptions
defaultOptions = GameOptions { _boardSize = 9,
                               _playerColor = Black,
                               _difficulty = 1,
                               _komi = -6.5 }

startingState :: GameOptions -> GameState
startingState options = GameState { _curBoard = empty_board,
                                    _score = options^.komi,
                                    _whoseTurn = Black,
                                    _passes = 0,
                                    _prevBoard = empty_board }
    where empty_board = makeBoard $ options ^. boardSize

getBoardSize :: IO Int
getBoardSize = do putStrLn "Choose board size: s - Small (9), m - Medium (13), l - Large (19):"
                  choice <- getLine
                  case map toLower choice of
                     s | s `elem` ["small",  "s", "9"]  -> return 9
                       | s `elem` ["medium", "m", "13"] -> return 13
                       | s `elem` ["large",  "l", "19"] -> return 19
                       | otherwise                      -> do putStrLn "Wrong! Try again"
                                                              getBoardSize

menuLoop :: [String] -> GameOptions -> IO ()
menuLoop args options = do
    putStrLn $ "Options: " ++ show options
    putStrLn "1. Single player"
    putStrLn "2. Multiplayer"
    putStrLn "3. Host LAN game"
    putStrLn "4. Join LAN game"
    putStrLn "5. Set board size"
    putStrLn "6. Exit"
    choice <- getLine
    case choice of
         "2"       -> do startMultiplayer options
                         menuLoop args options
         "5"       -> do newBoardSize <- getBoardSize
                         let newOptions = boardSize ^= newBoardSize $ options
                         menuLoop args newOptions
         "6"       -> return ()
         _         -> menuLoop args options

startMultiplayer :: GameOptions -> IO ()
startMultiplayer options =
    multiplayerWorker options $ startingState options

multiplayerWorker :: GameOptions -> GameState -> IO ()
multiplayerWorker options state = do
    newState <- makeTurn state
    let newState1 = whoseTurn ^= opposite (state^.whoseTurn) $ newState
    unless (newState1^.passes > 1) $ multiplayerWorker options newState1

makeTurn :: GameState -> IO GameState
makeTurn state = do
    putStrLn $ "Score: (-) White <--- " ++ show (state^.score) ++ " ---> Black (+)"
    putStr   $ Board.showAnnotated $ state^.curBoard
    printGroups Black
    printGroups White
    readTurn
    where printGroups col = putStr $ show col ++ " groups:\n" ++ showGroups (groups (state^.curBoard) col)
          readTurn = do
            let color = state^.whoseTurn
            putStrLn $ show color ++ "'s turn:"
            move <- getLine
            let parsed = case fromGoCoord move of
                                Nothing       -> Left "Failed to parse (hint: use coordinates, ex. D4)"
                                Just (-1, -1) -> Left "pass"
                                Just (i, j)   -> Right (i, j)
            let board = state^.curBoard
            let action = parsed >>= checkSize board >>= checkOccupied board >>= checkSuicide board >>= checkKo board
            case action of
                 Left err | err == "pass" -> return $ passes ^= (state^.passes + 1) $ state
                          | otherwise     -> putStrLn ("Wrong move: " ++ err) >> readTurn
                 Right (i, j)   -> return $ (prevBoard ^= board) . (curBoard ^= Board.replace board color i j) $ state

checkSize :: Board -> BoardCoord -> Either String BoardCoord
checkSize board (i, j) | i >= size || j >= size = Left "Coordinates are too big"
                       | otherwise              = Right (i, j)
    where size = getSize board

main :: IO ()
main = getArgs >>= \a -> menuLoop a defaultOptions

