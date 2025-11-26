module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.State

import Types
import Logic
import Render


main :: IO ()
main = play
        (InWindow "Haski" (screenWidth, screenHeight) (100, 100))
        (makeColor 0.32 0.33 0.05 1)
        60 -- fps
        initialGameState
        renderGame
        handleEvent
        updateGameWrapper


handleEvent :: Event -> GameState -> GameState
handleEvent event gs = execState (handleInputEvent event) gs

updateGameWrapper :: Float -> GameState -> GameState
updateGameWrapper dt gs = execState (updateGame dt) gs