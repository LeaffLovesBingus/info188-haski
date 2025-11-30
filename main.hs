module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Monad.State (execState)
import qualified Data.Map.Strict as Map
import Types
import Logic
import Render
import Assets
import MapLoader (loadMapFromJSON, loadGlobalCollisionShapesFromMap)
import System.Exit (exitSuccess)

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
    
    playIO
        (InWindow "Haski" (screenWidth, screenHeight) (100, 100))
        (makeColor 0.32 0.33 0.05 1)
        60
        initState
        (return . renderGame)
        handleEventIO
        updateGameWrapperIO

handleEventIO :: Event -> GameState -> IO GameState
handleEventIO event gs = do
    let newState = execState (handleInputEvent event) gs
    -- Verificar si debemos salir (escena especial)
    if shouldExit newState
        then exitSuccess
        else return newState

updateGameWrapperIO :: Float -> GameState -> IO GameState
updateGameWrapperIO dt gs = return $ execState (updateGame dt) gs

-- Verificar si el juego debe cerrarse
shouldExit :: GameState -> Bool
shouldExit gs = exitRequested gs