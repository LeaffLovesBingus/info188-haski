module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.State (execState)
import qualified Data.Map.Strict as Map
import Types
import Logic
import Render
import MapLoader (loadMapFromJSON, loadGlobalCollisionShapesFromMap)

main :: IO ()
main = do
    -- Cargar mapa
    (_tilesetsInfo, tileLayersLoaded, collisions) <- loadMapFromJSON "assets/map/mapa.JSON"
    
    -- Cargar shapes de colisión desde TSX
    shapesMap <- loadGlobalCollisionShapesFromMap "assets/map/mapa.JSON"
    
    -- Usar primera capa como tileMap, pero guardar todas las capas
    let allLayerTiles = map snd tileLayersLoaded
        firstLayerTiles = if null allLayerTiles
                          then []
                          else head allLayerTiles
        
        -- Crear estado inicial base
        initStateBase = initialGameState firstLayerTiles allLayerTiles collisions
        
        -- Añadir collision shapes
        initStateWithShapes = initStateBase { collisionShapes = shapesMap }
        
        -- NUEVO: Escanear y crear objetos destructibles
        initState = scanForDestructibles initStateWithShapes
    
    putStrLn $ "Objetos destructibles encontrados: " ++ show (length (destructibleObjects initState))
    
    play
        (InWindow "Haski" (screenWidth, screenHeight) (100, 100))
        (makeColor 0.32 0.33 0.05 1)
        60
        initState
        renderGame
        handleEvent
        updateGameWrapper

handleEvent :: Event -> GameState -> GameState
handleEvent event gs = execState (handleInputEvent event) gs

updateGameWrapper :: Float -> GameState -> GameState
updateGameWrapper dt gs = execState (updateGame dt) gs