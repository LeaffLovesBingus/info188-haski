module Render where

import Types
import Assets
import MapLoader (loadMapFromJSON, TilesetInfo, TileLayer, CollisionShape(..))
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Codec.Picture
import Codec.Picture.Types
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (try, IOException)
import System.FilePath (normalise)
import qualified Data.Array as Array
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Bits ((.&.))

-- Cache de capas (tile layers) leído desde el JSON (unsafePerformIO para no cambiar Types)
layersCache :: [TileLayer]
layersCache = unsafePerformIO $ do
    eres <- try (loadMapFromJSON "assets/map/mapa.JSON") :: IO (Either IOException ([TilesetInfo], [TileLayer], [[Bool]]))
    case eres of
        Right (_tilesets, layers, _coll) -> return layers
        Left _ -> return []
{-# NOINLINE layersCache #-}

-- Cargar tileset del fondo
-- Cargamos el tileset una sola vez usando unsagePerformIO
tilesetBackground :: Maybe (Image PixelRGBA8)
tilesetBackground = unsafePerformIO $ do
    result <- readImage "assets/ambiente/pasto.png"
    return $ case result of
        Left _ -> Nothing
        Right dynImg -> Just $ convertRGBA8 dynImg
{-# NOINLINE tilesetBackground #-}


-- Cargar sprite de flecha
arrowImage :: Maybe (Image PixelRGBA8)
arrowImage = unsafePerformIO $ do
    result <- readImage "assets/entidades/flecha.png"
    return $ case result of
        Left _ -> Nothing
        Right dynImg -> Just (convertRGBA8 dynImg)
{-# NOINLINE arrowImage #-}


arrowPicture :: Picture
arrowPicture = 
    case arrowImage of
        Just img -> fromImageRGBA8 img
        Nothing -> color red (circleSolid 4)


-- Crosshair personalizado
crosshairImage :: Maybe (Image PixelRGBA8)
crosshairImage = unsafePerformIO $ do
    result <- readImage "assets/crosshair.png"
    return $ case result of
        Left _ -> Nothing
        Right dynImg -> Just (convertRGBA8 dynImg)
{-# NOINLINE crosshairImage #-}


crosshairPicture :: Picture
crosshairPicture = 
  case crosshairImage of
    Just img -> fromImageRGBA8 img
    Nothing -> Blank


-- Cache de tiles pre-renderizados (FALLBACK antiguo)
tileCache :: Array.Array Int Picture
tileCache = unsafePerformIO $ do
    let tiles = [extractTileIO i | i <- [0..31]]
    Array.listArray(0, 31) <$> sequence tiles
{-# NOINLINE tileCache #-}

-- Nuevo: cache dinámico que intenta cargar todos los tilesets listados en mapa JSON.
-- Si falla, se mantiene el tileCache antiguo (fallback).
{-
Reemplaza la implementación anterior de dynamicTileCache/getTile por esta.
-}
tilesetCache :: [(Int, [Picture], Int, Int, Int)]
tilesetCache = unsafePerformIO $ do
    eres <- try (loadMapFromJSON "assets/map/mapa.JSON") :: IO (Either IOException ([MapLoader.TilesetInfo], [MapLoader.TileLayer], [[Bool]]))
    case eres of
        Left _ -> return []  -- fallback vacío -> seguirá usando tileCache en caso necesario
        Right (tilesetsInfo, _layers, _coll) -> do
            picsPer <- mapM (\(fg, imgPath, tw, th, cols) -> do
                                let p = normalise imgPath
                                ps <- loadTileset p (if tw <= 0 then 32 else tw) (if th <= 0 then 32 else th)
                                return (fg, ps, tw, th, cols)
                            ) tilesetsInfo
            -- ordenar por firstgid por si acaso
            return $ sortBy (comparing (\(fg,_,_,_,_) -> fg)) picsPer
{-# NOINLINE tilesetCache #-}

-- Buscar el tileset aplicable para un gid: el que tenga el mayor firstgid <= gid
findTilesetForGid :: Int -> Maybe (Int, [Picture], Int, Int, Int)
findTilesetForGid gid
    | gid <= 0 = Nothing
    | otherwise =
        let applicable = filter (\(fg,_,_,_,_) -> fg <= gid) tilesetCache
        in if null applicable then Nothing else Just (last (sortBy (comparing (\(fg,_,_,_,_) -> fg)) applicable))

-- Obtener Picture dado un GID (respeta firstgid)
getTileFromGid :: Int -> Picture
getTileFromGid gid
    | gid <= 0 = Blank
    | otherwise =
        case findTilesetForGid gid of
            Nothing -> Blank
            Just (firstgid, pics, tw, th, cols) ->
                let idx = gid - firstgid
                in if idx >= 0 && idx < length pics
                   then let pic = pics !! idx
                        in -- escalar en caso de que tileSize != tile width
                           let scaleFactor = tileSize / (fromIntegral (if tw <= 0 then 32 else tw))
                           in scale scaleFactor scaleFactor pic
                   else Blank


-- Extraer un tile específico del tileset (fallback antiguo)
extractTileIO :: Int -> IO Picture
extractTileIO tileIndex = return $ fromMaybe Blank $ do
    img <- tilesetBackground
    let tileRow = tileIndex `div` 8
        tileCol = tileIndex `mod` 8

        tileWidth = 32 :: Int
        tileHeight = 32 :: Int

        tilePic = extractRegion img (tileCol * tileWidth) (tileRow * tileHeight) tileWidth tileHeight
    
    fromImageRGBA8 <$> tilePic


-- Extraer una región rectangular de una imagen
extractRegion :: Image PixelRGBA8 -> Int -> Int -> Int -> Int -> Maybe (Image PixelRGBA8)
extractRegion img x y w h
    | x < 0 || y < 0 || x + w > imageWidth img || y + h > imageHeight img = Nothing
    | otherwise = Just $ generateImage getPixel w h
    where
        getPixel px py = pixelAt img (x + px) (y + py)


-- Renderizar el juego completo
renderGame :: GameState -> Picture
renderGame gs = pictures 
    [ renderBackground gs
    , renderPlayer gs
    , renderProjectiles gs
    , renderCursor gs
    -- , renderDebugCollisions gs  -- Debug: descomentar para ver cajas de colisión
    ]


-- DEBUG: Renderizar cajas de colisión visibles
renderDebugCollisions :: GameState -> Picture
renderDebugCollisions gs =
    let cam = cameraPos (camera gs)
        camX = fst cam
        camY = snd cam
        (px, py) = playerPos (player gs)
        h = playerCollisionHalfSize
        
        layers = allLayers gs
        firstLayer = if null layers then [] else head layers
        mapH = length firstLayer
        
        shapesMap = collisionShapes gs
        
        -- Dibujar caja del jugador (verde)
        playerBoxPic = translate (px - camX) (py - camY) $
                       color (makeColor 0 1 0 0.5) $
                       rectangleWire (2 * h) (2 * h)
        
        -- Convertir shape a AABB (misma lógica que Logic.hs)
        -- IMPORTANTE: Escalar de 32x32 a 64x64
        shapeScale :: Float
        shapeScale = tileSize / 32.0
        
        shapeToWorldAABB col row (CRect sx sy sw sh) =
            let sx' = sx * shapeScale
                sy' = sy * shapeScale
                sw' = sw * shapeScale
                sh' = sh * shapeScale
                tileX = fromIntegral col * tileSize
                tileY = fromIntegral (mapH - 1 - row) * tileSize
                worldX = tileX + sx'
                worldY = tileY + (tileSize - sy' - sh')
            in (worldX, worldY, sw', sh')
        shapeToWorldAABB col row (CPoly pts) =
            let scaledPts = map (\(px, py) -> (px * shapeScale, py * shapeScale)) pts
                xs = map fst scaledPts
                ys = map snd scaledPts
                minx = minimum xs
                maxx = maximum xs
                miny = minimum ys
                maxy = maximum ys
                tileX = fromIntegral col * tileSize
                tileY = fromIntegral (mapH - 1 - row) * tileSize
                worldY = tileY + (tileSize - maxy)
            in (tileX + minx, worldY, maxx - minx, maxy - miny)
        
        -- Rango visible
        visibleCols = [floor ((px - 200) / tileSize) .. ceiling ((px + 200) / tileSize)]
        visibleRows = [0 .. mapH - 1]
        
        normalizeGid gid = gid .&. 0x1FFFFFFF
        
        -- Dibujar todas las collision shapes visibles
        collisionPics = 
            [ translate (bx + bw/2 - camX) (by + bh/2 - camY) $
              color (makeColor 1 0 0 0.3) $
              rectangleSolid bw bh
            | layer <- layers
            , row <- visibleRows
            , row < length layer
            , col <- visibleCols  
            , col >= 0 && col < length (layer !! row)
            , let rawGid = (layer !! row) !! col
            , let gid = normalizeGid rawGid
            , gid > 0
            , Just shapes <- [Map.lookup gid shapesMap]
            , shape <- shapes
            , let (bx, by, bw, bh) = shapeToWorldAABB col row shape
            , bw > 0.5 && bh > 0.5
            ]
        
    in pictures (playerBoxPic : collisionPics)


-- Renderizar el fondo con tiles (AHORA dibuja todas las tile layers en orden)
renderBackground :: GameState -> Picture
renderBackground gs =
    let cam = cameraPos (camera gs)
        camX = fst cam
        camY = snd cam

        -- elegir capas: las del mapa si existen, si no usar el tileMap del estado (fallback)
        layersToDraw :: [(String, [[Int]])]
        layersToDraw = if null layersCache
                       then [("legacy", tileMap gs)]
                       else map (\(_, tiles) -> ("", tiles)) layersCache

        -- función que dibuja una capa completa
        renderLayerTiles :: [[Int]] -> Picture
        renderLayerTiles tiles =
            let screenWidthF = fromIntegral screenWidth
                screenHeightF = fromIntegral screenHeight

                -- número de filas del mapa (para convertir índices Tiled(top-origin) <-> mundo (bottom-origin))
                mapH = length tiles

                -- límites en coordenadas mundo visibles
                leftWorld   = camX - screenWidthF/2
                rightWorld  = camX + screenWidthF/2
                topWorld    = camY + screenHeightF/2
                bottomWorld = camY - screenHeightF/2

                -- convertir coordenadas mundo a índices de fila (rango de filas Tiled: 0..mapH-1, 0 = top)
                worldYToRow wy = mapH - 1 - floor (wy / tileSize)

                minTileX = floor (leftWorld / tileSize) - 1
                maxTileX = ceiling (rightWorld / tileSize) + 1

                r1 = worldYToRow topWorld
                r2 = worldYToRow bottomWorld
                minTileY = max 0 (min r1 r2 - 1)
                maxTileY = min (max 0 (mapH - 1)) (max r1 r2 + 1)

                visibleTiles = [ renderTile tiles x y camX camY
                               | x <- [minTileX..maxTileX],
                                 y <- [minTileY..maxTileY] ]
            in pictures visibleTiles

        layerPics = map (\(_, tiles) -> renderLayerTiles tiles) layersToDraw

    in pictures layerPics


-- Renderiza un tile individual
renderTile :: [[Int]] -> Int -> Int -> Float -> Float -> Picture
renderTile tiles x y camX camY = 
    let mapH = length tiles

        -- convertir índice de fila (y, con 0 = top) a coordenada mundo (0 = bottom)
        -- El tile se posiciona en su esquina bottom-left, pero Gloss centra el sprite
        -- Así que añadimos tileSize/2 para que el centro del sprite coincida con el centro del tile
        worldX = fromIntegral x * tileSize + tileSize / 2
        worldY = fromIntegral (mapH - 1 - y) * tileSize + tileSize / 2

        -- Posición en pantalla relativa a la cámara
        screenX = worldX - camX
        screenY = worldY - camY

        -- Obtener índice del tile 
        tileIndex = if y >= 0 && y < mapH && x >= 0 && (not (null tiles)) && x < length (head tiles)
                  then tiles !! y !! x
                  else -1

        -- Obtener el tile del tilesheet (usar el mapeo GID -> tileset)
        tilePic = if tileIndex <= 0 then Blank else getTileFromGid tileIndex

    in translate screenX screenY
        $ tilePic



-- Renderizar jugador
renderPlayer :: GameState -> Picture
renderPlayer gs = 
    let p = player gs
        cam = cameraPos (camera gs)
        (px, py) = playerPos p
        screenX = px - fst cam 
        screenY = py - snd cam 

        -- Determinar la animación en base a la velocidad
        (vx, vy) = playerVel p
        isMoving = vx /= 0 || vy /= 0
        anim = if isMoving then Walk else Idle 

        -- Asegurarse de que el frame está en rango
        dir = playerDir p
        frame = playerFrame p
        maxFrame = if anim == Idle then 1 else 3
        safeFrame = frame `mod` (maxFrame + 1)

        framePic = playerFrames Array.! (dir, anim, safeFrame)

    in translate screenX screenY 
        $ scale 2.0 2.0
        $ framePic


-- Renderizxar proyectiles
renderProjectiles :: GameState -> Picture
renderProjectiles gs = 
    let cam = cameraPos (camera gs)
        camX = fst cam
        camY = snd cam
        projs = projectiles gs

        renderProj proj = 
            let (px, py) = projPos proj
                (vx, vy) = projVel proj

                -- Posición relativa a la cámara
                screenX = px - camX
                screenY = py - camY

                -- Ángulo de la flecha
                angleRad = atan2 vy vx
                angleDeg = angleRad * 180 / pi

            in translate screenX screenY $
                rotate (-angleDeg) $
                scale 1.5 1.5 $
                arrowPicture

    in pictures (map renderProj projs)


renderCursor :: GameState -> Picture
renderCursor gs =
    let mPos = mousePos (inputState gs)
        (mx, my) = mPos
    in translate mx my 
        $ scale 0.3 0.3
        $ crosshairPicture