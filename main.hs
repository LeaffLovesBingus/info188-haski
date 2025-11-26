module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.State

import Types
import Logic
import Render
import MapLoader (loadMapFromJSON)

main :: IO ()
main = do
    -- ahora loadMapFromJSON devuelve (tilesetsInfo, tileLayers, collisionMap)
    ( _tilesetsInfo, tileLayersLoaded, collisions ) <- loadMapFromJSON "assets/map/mapa.JSON"

    -- tomar la primera tilelayer como tileMap legacy para initialGameState
    let firstLayerTiles :: [[Int]]
        firstLayerTiles = if null tileLayersLoaded then [] else snd (head tileLayersLoaded)

        initState = initialGameState firstLayerTiles collisions

    play
        (InWindow "Haski" (screenWidth, screenHeight) (100, 100))
        (makeColor 0.32 0.33 0.05 1)
        60 -- fps
        initState
        renderGame
        handleEvent
        updateGameWrapper


handleEvent :: Event -> GameState -> GameState
handleEvent event gs = execState (handleInputEvent event) gs

updateGameWrapper :: Float -> GameState -> GameState
updateGameWrapper dt gs = execState (updateGame dt) gs