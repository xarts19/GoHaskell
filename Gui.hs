module Gui
( mainWithGui
) where

import Board
import GameEngine
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Data.Lens.Lazy ( (^.) )
import Control.Concurrent
import Control.Concurrent.MVar()
import Control.Monad.State
import Data.Maybe
import System.Exit


mainWithGui :: GameOptions -> [String] -> IO ()
mainWithGui opts args = do
    exitFlag <- newEmptyMVar
    gameStateMVar <- newEmptyMVar
    boardClickMVar <- newEmptyMVar
    boardHoverMVar <- newEmptyMVar

    _ <- initGUI

    window <- windowNew
    set window [windowDefaultWidth := 700, windowDefaultHeight := 400,
                containerBorderWidth := 10]


    btnStartGame <- buttonNewWithLabel "New game"
    _ <- onClicked btnStartGame $ do
            _ <- forkIO (startMultiplayer window opts gameStateMVar boardClickMVar)
            return ()

    exitButton <- buttonNewWithLabel "Exit"
    _ <- exitButton `on` buttonActivated $ putMVar exitFlag ExitSuccess

    canvas <- drawingAreaNew
    widgetAddEvents canvas [ButtonPressMask, PointerMotionMask]
    _ <- canvas `on` exposeEvent $ canvasExposeHandler opts gameStateMVar boardHoverMVar
    _ <- canvas `on` buttonPressEvent $ canvasClickHandler opts boardClickMVar
    _ <- canvas `on` motionNotifyEvent $ mouseMotionHandler opts boardHoverMVar

    vBox <- vBoxNew False 0
    boxPackStart vBox btnStartGame PackNatural 0
    boxPackEnd vBox exitButton PackNatural 0

    hBox <- hBoxNew False 0
    boxPackStart hBox canvas PackGrow 0
    boxPackStart hBox vBox PackNatural 0

    containerAdd window hBox
    _ <- window `on` deleteEvent $ liftIO (putMVar exitFlag ExitSuccess) >>
                                   liftIO mainQuit >>
                                   return False

    widgetShowAll window
    _ <- forkOS mainGUI
    signal <- takeMVar exitFlag
    postGUIAsync mainQuit
    exitWith signal


startMultiplayer :: Window -> GameOptions -> MVar GameState -> MVar BoardCoord -> IO ()
startMultiplayer window opts gameStateMVar boardClickMVar = do
    let st = initState opts
    putMVar gameStateMVar st
    putStrLn $ "Game started"
    handleTurns window st gameStateMVar getMovePlayer1 getMovePlayer2
--    showScore opts st
    return ()
    where   getMovePlayer1 :: IO Move
            getMovePlayer1 = do coord <- takeMVar boardClickMVar
                                return $ Move coord
            getMovePlayer2 :: IO Move
            getMovePlayer2 = do coord <- takeMVar boardClickMVar
                                return $ Move coord


handleTurns :: Window -> GameState -> MVar GameState -> IO Move -> IO Move -> IO ()
handleTurns window st gameStateMVar getMovePlayer1 getMovePlayer2 = do
    let over = whyGameOver st
    case over of
        Just msg -> putStrLn $ "Game over: " ++ msg
        Nothing  -> do
            move <- getMovePlayer1
            let (result, newSt) = runState (makeMove move) st
            case result of
                Left str -> do putStrLn $ "Wrong move!!! " ++ str
                               handleTurns window st gameStateMVar getMovePlayer1 getMovePlayer2
                Right m ->  do putStrLn $ "Correct move!!! " ++ (showMove m)
                               _ <- swapMVar gameStateMVar newSt
                               widgetQueueDraw window
                               handleTurns window newSt gameStateMVar getMovePlayer2 getMovePlayer1


calcScale :: (Int, Int) -> Int -> (Double, Double, Int)
calcScale (width, height) numGrids = (totalSize, gridStep, numGrids)
    where width'  = fromIntegral width
          height' = fromIntegral height
          totalSize = (if width' < height' then width' else height') :: Double
          gridStep = totalSize / (fromIntegral numGrids + 1)


canvasExposeHandler :: GameOptions -> MVar GameState -> MVar BoardCoord -> EventM EExpose Bool
canvasExposeHandler opts gameStateMVar boardHoverMVar = do
    win <- eventWindow
    liftIO $ do
        sizes <- drawableGetSize win
        let (totalSize, gridStep, numGrids) = calcScale sizes (opts^.boardSize)

        -- Draw board using the cairo api
        renderWithDrawable win $ do
            drawBoard totalSize gridStep numGrids

        isGameInProgress <- liftM not $ isEmptyMVar gameStateMVar
        if isGameInProgress
            then do
                gameState <- readMVar gameStateMVar
                renderWithDrawable win $ do
                    drawStones (gameState^.curBoard) gridStep

                when (gameState^.curPlayer == opts^.pcPlayer) $ do
                    isMouseOnBoard <- liftM not $ isEmptyMVar boardHoverMVar
                    when isMouseOnBoard $ do
                        boardCoord <- readMVar boardHoverMVar
                        when (isNothing $ isMoveAllowed gameState (Move boardCoord)) $ do
                            renderWithDrawable win $ do
                                drawStonePreview boardCoord gridStep (opts^.pcPlayer)
            else return ()

    return False


canvasClickHandler :: GameOptions -> MVar BoardCoord -> EventM EButton Bool
canvasClickHandler opts boardClickMVar = do
    win <- eventWindow
    (x, y) <- eventCoordinates
    liftIO $ do
        sizes <- drawableGetSize win
        let (_, gridStep, numGrids) = calcScale sizes (opts^.boardSize)
            i = truncate ((y - gridStep / 2) / gridStep)
            j = truncate ((x - gridStep / 2) / gridStep)
        when (i >= 0 && i < numGrids && j >= 0 && j < numGrids) $ do
            putStrLn $ "Recieved click at: " ++ show (i, j)
            tryPutMVar boardClickMVar (i, j) >> return ()
    return False


mouseMotionHandler :: GameOptions -> MVar BoardCoord -> EventM EMotion Bool
mouseMotionHandler opts boardHoverMVar = do
    win <- eventWindow
    (x, y) <- eventCoordinates
    liftIO $ do
        sizes <- drawableGetSize win
        let (_, gridStep, numGrids) = calcScale sizes (opts^.boardSize)
            i = truncate ((y - gridStep / 2) / gridStep)
            j = truncate ((x - gridStep / 2) / gridStep)
        _ <- tryTakeMVar boardHoverMVar
        when (i >= 0 && i < numGrids && j >= 0 && j < numGrids) $ do
            tryPutMVar boardHoverMVar (i, j) >> return ()
    return False


drawBoard :: Double -> Double -> Int -> Render ()
drawBoard _ _ 0 = return ()
drawBoard totalSize gridStep i = do
    --setSourceRGB 0.5 0.5 0.5
    --paint
    setSourceRGB 0 0 0
    setLineWidth 1
    let start = gridStep
        end = totalSize - gridStep
        point = fromIntegral i
    moveTo (gridStep*point) start
    lineTo (gridStep*point) end
    stroke
    moveTo start  (gridStep*point)
    lineTo end (gridStep*point)
    stroke
    drawBoard totalSize gridStep (i-1)


drawStones :: Board -> Double -> Render ()
drawStones board gridStep = do
    sequence_ $ map drawStone $ boardToList board
    where
        drawStone (_, _, Empty) = return ()
        drawStone (i, j, color) = do
            case color of
                Black -> setSourceRGB 0 0 0
                White -> setSourceRGB 1 1 1
                Empty -> setSourceRGB 1 0 0
            arc ((fromIntegral i + 1) * gridStep)
                ((fromIntegral j + 1) * gridStep)
                (gridStep / 2) 0 (2 * pi)
            fill


drawStonePreview :: (Int, Int) -> Double -> Player -> Render ()
drawStonePreview (i, j) gridStep player = do
    case player of
        PlayerBlack -> setSourceRGBA 0 0 0 0.7
        PlayerWhite -> setSourceRGBA 1 1 1 0.7
    arc ((fromIntegral i + 1) * gridStep)
        ((fromIntegral j + 1) * gridStep)
        (gridStep / 2) 0 (2 * pi)
    fill



