{-# LANGUAGE TemplateHaskell #-}

module Game
( GameOptions(..)
-- helpers
, defaultOptions
, showOptions
-- accessors
, boardSize
, playerColor
, difficulty
, komi

, GameState
-- helpers
, initGame
, readMove
, makeMove
, showStats
-- accessors
, gameOver

, pOpposite

, Move(..)
, showMove

, Player(..)
) where


import Board
import Rules
import Data.Char ( toLower )
import Data.Lens.Lazy ( (^=), (^.), (^+=), (^%=) )
import Data.Lens.Template ( makeLenses )
import Data.Maybe
import Control.Monad ()
import Control.Monad.State


data Move = Pass | Resign | Move BoardCoord deriving (Show, Eq, Read)
data Player = PBlack | PWhite deriving (Show, Eq, Read)


showMove :: Move -> String
showMove Pass = "pass"
showMove Resign = "resign"
showMove (Move m) = fromMaybe "Error" $ toGoCoord m


pColor :: Player -> Cell
pColor PBlack = Black
pColor PWhite = White


pOpposite :: Player -> Player
pOpposite PBlack = PWhite
pOpposite PWhite = PBlack


type Territory = Float
type Stones = Float
type Bonus = Float
type ScoreSingle = (Territory, Stones, Bonus)
type ScoreBlack = ScoreSingle
type ScoreWhite = ScoreSingle
type Score = (ScoreBlack, ScoreWhite)


showScore :: Score -> String
showScore sc@(black, white) = "Score (Territory, Captured, Bonus): White " ++ show white ++
                               " |-- [[ " ++ show (getScore sc) ++ " ]] --> Black " ++ show black


getScore :: Score -> Float
getScore ((tb, sb, bb), (tw, sw, bw)) = tb + sb + bb - tw - sw - bw


whoWins :: Score -> String
whoWins sc = (if scFl > 0 then "Black" else "White") ++ " wins by " ++ show (abs scFl) ++ " points"
    where scFl = getScore sc


updateStones :: (Int, Int) -> Score -> Score
updateStones (b, w) ((tb, sb, bb), (tw, sw, bw)) = ((tb, sb + fromIntegral b, bb), (tw, sw + fromIntegral w, bw))


setTerritory :: (Int, Int) -> Score -> Score
setTerritory (b, w) ((_, sb, bb), (_, sw, bw)) = ((fromIntegral b, sb, bb), (fromIntegral w, sw, bw))


data GameOptions = GameOptions
                    { _boardSize :: Int
                    , _playerColor :: Player
                    , _difficulty :: Int
                    , _komi :: Float } deriving (Show, Read)


data GameState = GameState
                  { _curBoard :: Board.Board
                  , _score :: Score
                  , _whoseTurn :: Player
                  , _allMoves :: [Move]
                  , _passes :: Int
                  , _prevBoard :: Board.Board
                  , _gameOver :: Bool }

$( makeLenses [''GameOptions,''GameState] )


showOptions :: GameOptions -> String
showOptions options = concat ["Size - ", show $ options^.boardSize, "; ",
                      "Player color - ", show $ options^.playerColor, "; ",
                        "Difficulty - ", show $ options^.difficulty, "; ",
                              "Komi - ", show $ options^.komi]


defaultOptions :: GameOptions
defaultOptions = GameOptions { _boardSize = 9
                             , _playerColor = PBlack
                             , _difficulty = 1
                             , _komi = 6.5 }


initState :: GameOptions -> GameState
initState options = GameState { _curBoard = empty_board
                              , _score = ((0, 0, 0), (0, 0, options^.komi))
                              , _whoseTurn = PBlack
                              , _allMoves = []
                              , _passes = 0
                              , _prevBoard = empty_board
                              , _gameOver = False }
    where empty_board = makeBoard $ options ^. boardSize


initGame :: GameOptions -> IO GameState
initGame opts = do
    putStrLn "Game has started"
    return $ initState opts


readMove :: GameState -> IO Move
readMove st = do
    move <- getLine
    case readMove' move >>= checkMove st of
        Left err     -> putStrLn ("Wrong move: " ++ err) >> readMove st
        Right mv     -> return mv


makeMove :: GameState -> Move -> IO GameState
makeMove st move = do
    let curPlayer = pColor (st^.whoseTurn)
    newState <- makeMove' st
    let finished = newState^.passes > 1
    let resigned = Resign == head (newState^.allMoves)

    if finished || resigned
       then do
           putStrLn $ "Game over: " ++ if finished then "both players passed"
                                                   else show curPlayer ++ " resigned"
           putStrLn $ if resigned then (show . opposite) curPlayer ++ " won!"
                                  else whoWins $ newState^.score
           return $ gameOver ^= True $ newState
       else
           return $ whoseTurn ^%= pOpposite $ newState

    where makeMove' curState = do
            let newSt = allMoves ^%= (move:) $ curState
            case move of
                 Pass       -> return $ passes ^+= 1 $ newSt
                 Resign     -> return newSt
                 Move coord -> return $ execState (execMove coord) newSt


readMove' :: String -> Either String Move
readMove' str | lstr == "p" || lstr == "pass"   = Right Pass
              | lstr == "r" || lstr == "resign" = Right Resign
              | otherwise = case fromGoCoord str of
                                 Nothing    -> Left "Failed to parse (hint: use coordinates, ex. D4, pass, resign)"
                                 Just coord -> Right $ Move coord
    where lstr = map toLower str


showStats :: GameState -> IO ()
showStats st = do
        putStrLn $ showScore $ st^.score
        putStr $ Board.showAnnotated $ st^.curBoard
        printGroups Black
        printGroups White
        return ()
    where printGroups col = putStr $ show col ++ " chains:\n" ++ showChainsWithLib (st^.curBoard) (chains (st^.curBoard) col)


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
    updateScore $ if color == Black then (length capt, 0) else (0, length capt)
    where getColor            = do s <- get
                                   return $ pColor $ s^.whoseTurn
          captured col        = do s <- get
                                   return $ getCaptured (s^.curBoard) $ opposite col
          saveBoard           = do s <- get
                                   put $ prevBoard ^= (s^.curBoard) $ s
          addStone col        = modify $ curBoard ^%= (\b -> replace b col [coord])
          removeCaptured capt = modify $ curBoard ^%= (\b -> replace b Empty capt)
          updateScore stns    = do s <- get
                                   let tb = territory (s^.curBoard) Black
                                   let tw = territory (s^.curBoard) White
                                   modify $ (score ^%= updateStones stns) . (score ^%= setTerritory (tb, tw))


checkSize :: Board -> BoardCoord -> Either String BoardCoord
checkSize board (i, j) | i >= size || j >= size = Left "Coordinates are too big"
                       | otherwise              = Right (i, j)
    where size = getSize board


