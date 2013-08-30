module Gui
( mainWithGui
) where

import Board
import Game
import GameEngine
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Data.Lens.Lazy ( (^=), (^.), (^+=), (^%=) )
import Control.Concurrent
import Control.Concurrent.MVar
import System.Exit


mainWithGui :: GameOptions -> [String] -> IO ()
mainWithGui opts args = do
    exitFlag <- newEmptyMVar

    _ <- initGUI

    btnStartGame <- buttonNewWithLabel "New game"
    _ <- onClicked btnStartGame $ do
        _ <- forkIO (startMultiplayer opts)
        return ()

    exitButton <- buttonNewWithLabel "Exit"
    _ <- exitButton `on` buttonActivated $ putMVar exitFlag ExitSuccess

    canvas <- drawingAreaNew
    _ <- canvas `on` exposeEvent $ updateCanvas opts

    vBox <- vBoxNew False 0
    boxPackStart vBox btnStartGame PackNatural 0
    boxPackEnd vBox exitButton PackNatural 0

    hBox <- hBoxNew False 0
    boxPackStart hBox canvas PackGrow 0
    boxPackStart hBox vBox PackNatural 0

    window <- windowNew
    containerAdd window hBox
    _ <- onDelete window (\_ -> return False)
    _ <- onDestroy window mainQuit
    set window [windowDefaultWidth := 700, windowDefaultHeight := 400,
                containerBorderWidth := 10]
    widgetShowAll window
    _ <- forkOS mainGUI
    signal <- takeMVar exitFlag
    postGUIAsync mainQuit
    exitWith signal
    

startMultiplayer :: GameOptions -> IO ()
startMultiplayer opts = do
    st <- initGame opts
    handleTurns st Black
    return ()


handleTurns :: GameState -> Cell -> IO ()
handleTurns st color = do
    showStats st
    putStrLn $ show color ++ " moves: "


updateCanvas :: GameOptions -> EventM EExpose Bool
updateCanvas opts = do
    win <- eventWindow
    liftIO $ do
        (width',height') <- drawableGetSize win
        let width  = realToFrac width'
            height = realToFrac height'
            bSize = fromIntegral $ opts^.boardSize

        -- Draw using the cairo api
        renderWithDrawable win $ do
            setSourceRGB 0 0 0
            setLineWidth 1
            let size = if width < height then width else height
            drawBoard size (size/(bSize+1)) bSize

    return False

    where drawBoard _ _ 0 = return ()
          drawBoard size step i = do
              let st = step
                  end = size - step
              moveTo (step*i) st
              lineTo (step*i) end
              stroke
              moveTo st  (step*i)
              lineTo end (step*i)
              stroke
              drawBoard size step (i-1)

