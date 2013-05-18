{-# LANGUAGE TemplateHaskell #-}

import Board
import System.Environment
import Data.Char ( toLower, toUpper, isSpace )
import Data.Lens.Lazy ( (^=), (^.) )
import Data.Lens.Template ( makeLenses )
import Data.Maybe
import Data.List ( elemIndex )
import Control.Monad ( when, unless )

data GameOptions = GameOptions
                    { _boardSize :: Board.BoardType
                    , _playerCell :: Board.Cell
                    , _difficulty :: Int
                    , _komi :: Float }

$( makeLenses [''GameOptions] )

instance Show GameOptions where
    show options = concat ["Size - ", show $ options ^. boardSize, "; ",
                           "Player color - ", show $ options ^. playerCell, "; ",
                           "Difficulty - ", show $ options ^. difficulty, "; ",
                           "Komi - ", show $ options ^. komi]

defaultOptions :: GameOptions
defaultOptions = GameOptions { _boardSize = Board.Small,
                               _playerCell = Board.Black,
                               _difficulty = 1,
                               _komi = 6.5 }

getBoardSize :: IO Board.BoardType
getBoardSize = do putStrLn "Choose board size: s - Small (9), m - Medium (13), l - Large (19):"
                  choice <- getLine
                  case map toLower choice of
                     s | s `elem` ["small",  "s", "9"]  -> return Board.Small
                       | s `elem` ["medium", "m", "13"] -> return Board.Medium
                       | s `elem` ["large",  "l", "19"] -> return Board.Large
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
    multiplayerWorker options (Board.makeBoard $ options ^. boardSize) False

multiplayerWorker :: GameOptions -> Board.Board -> Bool -> IO ()
multiplayerWorker options board last_passed = do
    let color1 = options ^. playerCell
    let color2 = Board.opposite color1
    (newBoard1, passed1) <- makeTurn board color1 color2 color1
    if not (last_passed && passed1)
       then do
            (newBoard2, passed2) <- makeTurn newBoard1 color1 color2 color2
            let isFinished = passed1 && passed2
            unless isFinished $ multiplayerWorker options newBoard2 passed2
       else return ()

makeTurn :: Board.Board -> Board.Cell -> Board.Cell -> Board.Cell -> IO (Board.Board, Bool)
makeTurn board color1 color2 color = do
    putStrLn $ "Player 1: " ++ show color1 ++ "; Player 2: " ++ show color2
    putStrLn $ Board.showAnnotated board
    readTurn
    where readTurn = do
            putStrLn $ "Player " ++ (if color1 == color then "1" else "2") ++ " (" ++ show color ++ ") turn:"
            move <- getLine
            case parseTurn move of
                 Nothing       -> do putStrLn "Wrong move specifier, use LetterNumber, e. g. D4 or D 4"
                                     readTurn
                 Just (-1, -1) -> return (board, True)
                 Just (i, j)   -> return (Board.replace board color i j, False)

-- Nothing == wrong turn specification; Just (-1, -1) == pass
parseTurn :: String -> Maybe (Int, Int)
parseTurn "" = Nothing
parseTurn str@(ch:rest)
                | map toLower str == "pass" = Just (-1, -1)
                | otherwise = maybeTuple j i
    where i = elemIndex (toUpper ch) Board.letterCoords
          j = maybeRead rest

maybeTuple :: Maybe a -> Maybe b -> Maybe (a, b)
maybeTuple (Just a) (Just b) = Just (a, b)
maybeTuple Nothing _         = Nothing
maybeTuple _ Nothing         = Nothing

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . dropWhile isSpace . snd) . reads

main :: IO ()
main = getArgs >>= \a -> menuLoop a defaultOptions

