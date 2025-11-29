module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.State (execState)
import qualified Data.Map.Strict as Map
import Types
import Logic
import Render
import Assets
import MapLoader (loadMapFromJSON, loadGlobalCollisionShapesFromMap)

main :: IO ()
main = do
    -- Cargar mapa
    (tilesetsInfo, tileLayersLoaded, collisions) <- loadMapFromJSON "assets/map/mapa.JSON"
    
    -- DEBUG: Mostrar tilesets cargados
    putStrLn "Tilesets cargados:"
    mapM_ (\(fg, path, _, _, _) -> putStrLn $ "  firstgid=" ++ show fg ++ ", path=" ++ path) tilesetsInfo
    
    -- Cargar shapes de colisión desde TSX
    shapesMap <- loadGlobalCollisionShapesFromMap "assets/map/mapa.JSON"
    
    -- DEBUG: Mostrar shapes cargadas
    putStrLn $ "Shapes de colisión cargadas: " ++ show (Map.size shapesMap) ++ " GIDs"
    
    -- Usar primera capa como tileMap, pero guardar todas las capas
    let allLayerTiles = map snd tileLayersLoaded
        firstLayerTiles = if null allLayerTiles then [] else head allLayerTiles

    -- DEBUG: Mostrar capas
    putStrLn $ "Capas cargadas: " ++ show (length allLayerTiles)

    -- Crear estado inicial base
    let initStateBase = initialGameState firstLayerTiles allLayerTiles collisions
        -- Añadir collision shapes
        initStateWithShapes = initStateBase { collisionShapes = shapesMap }
        -- NUEVO: Escanear y crear objetos destructibles
        initState = scanForDestructibles initStateWithShapes
    
    putStrLn $ "Objetos destructibles encontrados: " ++ show (length (destructibleObjects initState))
    
    -- DEBUG: Mostrar detalles de objetos encontrados
    mapM_ (\obj -> putStrLn $ "  - GID: " ++ show (destGid obj) ++ 
                             ", Pos: " ++ show (destTilePos obj) ++
                             ", Health: " ++ show (destHealth obj)) 
          (destructibleObjects initState)
    
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