module Gui
( mainWithGui
) where

import Board
import Game
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo


mainWithGui :: GameOptions -> [String] -> IO()
mainWithGui opts args = do
    _ <- initGUI

    btnStartGame <- buttonNewWithLabel "New game"
    _ <- onClicked btnStartGame (startMultiplayer opts)

    canvas <- drawingAreaNew
    _ <- canvas `on` exposeEvent $ updateCanvas

    vBox <- vBoxNew False 0
    boxPackStart vBox btnStartGame PackNatural 0

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
    mainGUI


startMultiplayer :: GameOptions -> IO ()
startMultiplayer opts = do
    st <- initGame opts
    handleTurns st Black
    return ()


handleTurns :: GameState -> Cell -> IO ()
handleTurns st color = do
    showStats st
    putStrLn $ show color ++ " moves: "


updateCanvas :: EventM EExpose Bool
updateCanvas = do
    win <- eventWindow
    liftIO $ do
        (width',height') <- drawableGetSize win
        let width  = realToFrac width'
            height = realToFrac height'
            bSize = 9

        -- Draw using the cairo api
        renderWithDrawable win $ do
            setSourceRGB 0 0 0
            setLineWidth 1
            let size = if width < height then width else height
            drawBoard size (size/(bSize+1)) bSize

    return True

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

