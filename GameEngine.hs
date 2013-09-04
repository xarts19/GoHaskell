{-# LANGUAGE TemplateHaskell #-}

module GameEngine
( Move(..)
, Player(..)

, GameOptions(..)
, boardSize
, pcPlayer
, defaultOptions

, GameState(..)
, curBoard
, allMoves
, curPlayer
, initState

, isMoveAllowed
, makeMove
, showMove
, whyGameOver

) where


import Board
import Rules
import Data.Lens.Lazy ( (^=), (^.), (^+=), (^%=) )
import Data.Lens.Template ( makeLenses )
import Control.Monad.State
import Data.Maybe
import Data.Monoid


data Move = Pass | Resign | Move BoardCoord deriving (Show, Eq, Read)
data Player = PlayerBlack | PlayerWhite deriving (Show, Eq, Read)

data GameState = GameState
                  { _curBoard :: Board.Board
                  , _prevBoard :: Board.Board
                  , _allMoves :: [(Move, Player)]   -- new moves are appended to the front
                  , _curPlayer :: Player
                  , _numCapturedBlack :: Int
                  , _numCapturedWhite :: Int }

data GameOptions = GameOptions
                    { _boardSize :: Int
                    , _pcPlayer :: Player
                    , _difficulty :: Int
                    , _komi :: Float } deriving (Show, Read)

$( makeLenses [''GameState,''GameOptions] )

type FailReason = String

defaultOptions :: GameOptions
defaultOptions = GameOptions { _boardSize = 9
                             , _pcPlayer = PlayerBlack
                             , _difficulty = 1
                             , _komi = 6.5 }


initState :: GameOptions -> GameState
initState options = GameState {   _curBoard = empty_board
                                , _prevBoard = empty_board
                                , _allMoves = []
                                , _curPlayer = PlayerBlack
                                , _numCapturedBlack = 0
                                , _numCapturedWhite = 0 }
    where empty_board = makeBoard $ options ^. boardSize


toStoneColor :: Player -> Cell
toStoneColor PlayerBlack = Black
toStoneColor PlayerWhite = White


opposite :: Player -> Player
opposite PlayerBlack = PlayerWhite
opposite PlayerWhite = PlayerBlack


showMove :: Move -> String
showMove Pass = "pass"
showMove Resign = "resign"
showMove (Move m) = fromMaybe "Error" $ toGoCoord m


applyMove :: Move -> State GameState ()
applyMove move = do
    st <- get
    let player = st^.curPlayer
        board = st^.curBoard
        color = toStoneColor $ player
    case move of
        Move coord -> do
            modify $ prevBoard ^= board
            modify $ curBoard ^%= (\b -> replace b color [coord])
            let capturedStones = getCaptured board (Board.opposite color)
            case player of
                PlayerBlack -> modify $ numCapturedWhite ^+= (length capturedStones)
                PlayerWhite -> modify $ numCapturedBlack ^+= (length capturedStones)
            modify $ curBoard ^%= (\b -> replace b Empty capturedStones)
        _          -> return ()
    modify $ curPlayer ^%= GameEngine.opposite
    modify $ allMoves ^%= ((move, player):)


isMoveAllowed :: GameState -> Move -> Maybe FailReason
isMoveAllowed st (Move coord) =
    let board = st^.curBoard
        color = toStoneColor $ st^.curPlayer
    in  whyGameOver st <>
        checkBounds board coord <>
        checkOccupied board coord <>
        checkSuicide board color coord <> 
        checkKo (st^.prevBoard) board color coord
isMoveAllowed st _ = whyGameOver st
        

makeMove :: Move -> State GameState (Either FailReason Move)
makeMove move = do
    st <- get
    case isMoveAllowed st move of
        Just err -> return $ Left err
        Nothing  -> do applyMove move
                       return $ Right move


whyGameOver :: GameState -> Maybe String
whyGameOver st = check' moves <> noEmptyCells
    where moves = st^.allMoves
          check' ((Resign, _):_) = Just "Player resigned"
          check' ((Pass, _):(Pass, _):_) = Just "Both players passed"
          check' _ = Nothing
          noEmptyCells = Nothing -- TODO: implement


