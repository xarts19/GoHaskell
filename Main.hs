{-# LANGUAGE TemplateHaskell #-}

import Board
import Rules
import GoNetwork
import System.Environment
import Data.Char ( toLower )
import Data.Lens.Lazy ( (^=), (^.), (^+=), (^%=) )
import Data.Lens.Template ( makeLenses )
import Control.Monad ()
import Control.Monad.State

data Move = Pass | Resign | Move BoardCoord deriving (Show, Eq)
data Player = PBlack | PWhite deriving (Show, Eq)


pColor :: Player -> Cell
pColor PBlack = Black
pColor PWhite = White


pOpposite :: Player -> Player
pOpposite PBlack = PWhite
pOpposite PWhite = PBlack


data GameOptions = GameOptions
                    { _boardSize :: Int
                    , _playerColor :: Cell
                    , _difficulty :: Int
                    , _komi :: Float }


data GameState = GameState
                  { _curBoard :: Board.Board
                  , _scoreStones :: Float    -- Black : positive, White : negative
                  , _whoseTurn :: Player
                  , _allMoves :: [Move]
                  , _passes :: Int
                  , _prevBoard :: Board.Board }

$( makeLenses [''GameOptions,''GameState] )


instance Show GameOptions where
    show options = concat ["Size - ", show $ options^.boardSize, "; ",
                           "Player color - ", show $ options^.playerColor, "; ",
                           "Difficulty - ", show $ options^.difficulty, "; ",
                           "Komi - ", show $ options^.komi]


defaultOptions :: GameOptions
defaultOptions = GameOptions { _boardSize = 9
                             , _playerColor = Black
                             , _difficulty = 1
                             , _komi = -6.5 }


startingState :: GameOptions -> GameState
startingState options = GameState { _curBoard = empty_board
                                  , _scoreStones = options^.komi
                                  , _whoseTurn = PBlack
                                  , _allMoves = []
                                  , _passes = 0
                                  , _prevBoard = empty_board }
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


menuLoop :: GameOptions -> [String] -> IO ()
menuLoop options args = do
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
                         menuLoop options args
         "5"       -> do newBoardSize <- getBoardSize
                         let newOptions = boardSize ^= newBoardSize $ options
                         menuLoop newOptions args
         "6"       -> return ()
         _         -> menuLoop options args


startMultiplayer :: GameOptions -> IO ()
startMultiplayer options =
    multiplayerWorker options $ startingState options


multiplayerWorker :: GameOptions -> GameState -> IO ()
multiplayerWorker options st = do
    newState <- makeTurn st
    let finished = newState^.passes > 1
    let resigned = Resign == head (newState^.allMoves)
    let colStr = (show . pColor) (st^.whoseTurn)
    if finished || resigned
       then do
           putStrLn $ "Game over: " ++ if finished then "both players passed"
                                                   else colStr ++ " resigned"
           let score = fromIntegral (territory (newState^.curBoard) Black)
                     - fromIntegral (territory (newState^.curBoard) White)
                     - newState^.scoreStones
           putStrLn $ "Score = " ++ show score ++ "; " ++ if score > 0 then "Black" else "White" ++ " has won!"
       else
           let newState1 = whoseTurn ^%= pOpposite $ newState
           in multiplayerWorker options newState1


makeTurn :: GameState -> IO GameState
makeTurn st = do
        putStrLn $ "Territory: White - " ++ show (territory (st^.curBoard) White)
                         ++ "; Black - " ++ show (territory (st^.curBoard) Black)
        putStrLn $ "Score: (-) White <--- " ++ show (st^.scoreStones) ++ " ---> Black (+)"
        putStr $ Board.showAnnotated $ st^.curBoard
        printGroups Black
        printGroups White
        let colStr = (show . pColor) (st^.whoseTurn)
        putStrLn $ colStr ++ "'s turn:"
        readTurn st
    where printGroups col = putStr $ show col ++ " chains:\n" ++ showChainsWithLib (st^.curBoard) (chains (st^.curBoard) col)


readTurn :: GameState -> IO GameState
readTurn st = do
    move <- getLine
    case readMove move >>= checkMove st of
        Left err     -> putStrLn ("Wrong move: " ++ err) >> readTurn st
        Right mv -> do
            let newSt = allMoves ^%= (mv:) $ st
            case mv of
                Pass       -> return $ passes ^+= 1 $ newSt
                Resign     -> return newSt
                Move coord -> return $ execState (execMove coord) newSt


readMove :: String -> Either String Move
readMove str | lstr == "p" || lstr == "pass"   = Right Pass
             | lstr == "r" || lstr == "resign" = Right Resign
             | otherwise = case fromGoCoord str of
                                Nothing    -> Left "Failed to parse (hint: use coordinates, ex. D4)"
                                Just coord -> Right $ Move coord
    where lstr = map toLower str


checkMove :: GameState -> Move -> Either String Move
checkMove _ Pass          = Right Pass
checkMove _ Resign        = Right Resign
checkMove st (Move coord) = case checked of
                                 Left err -> Left err
                                 Right m  -> Right $ Move m
    where board = st^.curBoard
          color = pColor (st^.whoseTurn)
          checked = Right coord >>= checkSize board >>= checkOccupied board
                                >>= checkSuicide board color >>= checkKo (st^.prevBoard) board color


execMove :: BoardCoord -> State GameState ()
execMove coord = do
    color <- getColor
    saveBoard
    addStone color
    capt <- captured color
    removeCaptured capt
    updateScore (if color == Black then length capt else - length capt)
    where getColor            = do s <- get
                                   return $ pColor $ s^.whoseTurn
          captured col        = do s <- get
                                   return $ getCaptured (s^.curBoard) $ opposite col
          saveBoard           = do s <- get
                                   put $ prevBoard ^= (s^.curBoard) $ s
          addStone col        = modify $ curBoard ^%= (\b -> replace b col [coord])
          removeCaptured capt = modify $ curBoard ^%= (\b -> replace b Empty capt)
          updateScore val     = modify $ scoreStones ^+= fromIntegral val


checkSize :: Board -> BoardCoord -> Either String BoardCoord
checkSize board (i, j) | i >= size || j >= size = Left "Coordinates are too big"
                       | otherwise              = Right (i, j)
    where size = getSize board


main :: IO ()
main = getArgs >>= menuLoop defaultOptions

