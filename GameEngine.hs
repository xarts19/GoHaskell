{-# LANGUAGE TemplateHaskell #-}

module GameEngine
( GameState
-- helpers
, makeMove
-- accessors
, Move(..)
, Player(..)
) where

data Move = Pass | Resign | Move BoardCoord deriving (Show, Eq, Read)
data Player = PlayerBlack | PlayerWhite deriving (Show, Eq, Read)

data GameState = GameState
                  { _curBoard :: Board.Board
                  , _prevBoard :: Board.Board
                  , _allMoves :: [Move]
                  , _curPlayer :: Player
                  , _numCapturedBlack :: Int
                  , _numCapturedWhite :: Int }

$( makeLenses [''GameState] )

type FailReason = String


toStoneColor :: Player -> Cell
toStoneColor PlayerBlack = Black
toStoneColor PlayerWhite = White


checkMove :: GameState -> Move -> Either String Move
checkMove _ Pass          = Right Pass
checkMove _ Resign        = Right Resign
checkMove st (Move coord) = case checked of
                                 Left err -> Left err
                                 Right m  -> Right $ Move m
    where board = st^.curBoard
          color = toStoneColor (st^.curPlayer)
          checked = Right coord >>= checkSize board >>= checkOccupied board
                                >>= checkSuicide board color >>= checkKo (st^.prevBoard) board color


makeMove :: Move -> State GameState (Either FailReason Move)
makeMove move = do
    st <- get
    let isBlacksTurn = (length $ st^.allMoves) % 2 == 0
    let boardNow = st^.curBoard
    case move of
        
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

checkSize :: ReaderT GameState Either String BoardCoord



