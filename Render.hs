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
-- Las capas son: 0=Fondo, 1=Puerta, 2=Escaleras, 3=Plantas, 4=Props
-- El jugador se dibuja DESPUÉS de la capa 2 (debajo de plantas y props)
renderGame :: GameState -> Picture
renderGame gs = 
    let cam = cameraPos (camera gs)
        camX = fst cam
        camY = snd cam
        
        -- Obtener las capas
        allLayersList = if null layersCache
                        then [("legacy", tileMap gs)]
                        else map (\(name, tiles) -> (name, tiles)) layersCache
        
        -- Dividir capas: las primeras 3 (índices 0,1,2) van debajo del jugador
        -- las restantes (3,4) van encima
        (layersBelow, layersAbove) = splitAt 3 allLayersList
        
        -- Renderizar capas
        renderLayers layers = pictures $ map (\(_, tiles) -> renderLayerTiles tiles camX camY) layers
        
    in pictures 
        [ renderLayers layersBelow      -- Capas 0,1,2 (Fondo, Puerta, Escaleras)
        , renderWorldItems gs
        , renderPlayer gs               -- Jugador
        , renderProjectiles gs
        , renderLayers layersAbove      -- Capas 3,4 (Plantas, Props) - encima del jugador
        , renderCursor gs
        --, renderDebugCollisions gs      -- Debug
        ]


-- Renderizar una capa de tiles
renderLayerTiles :: [[Int]] -> Float -> Float -> Picture
renderLayerTiles tiles camX camY =
    let screenWidthF = fromIntegral screenWidth
        screenHeightF = fromIntegral screenHeight
        mapH = length tiles
        
        leftWorld   = camX - screenWidthF/2
        rightWorld  = camX + screenWidthF/2
        topWorld    = camY + screenHeightF/2
        bottomWorld = camY - screenHeightF/2
        
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


-- DEBUG: Renderizar cajas de colisión visibles
renderDebugCollisions :: GameState -> Picture
renderDebugCollisions gs =
    let cam = cameraPos (camera gs)
        camX = fst cam
        camY = snd cam
        (px, py) = playerPos (player gs)
        h = playerCollisionHalfSize
        -- Posición de colisión con offset (en los pies)
        collisionY = py + playerCollisionOffsetY
        
        layers = allLayers gs
        firstLayer = if null layers then [] else head layers
        mapH = length firstLayer
        
        shapesMap = collisionShapes gs
        
        -- Dibujar caja del jugador (verde) - en los pies
        playerBoxPic = translate (px - camX) (collisionY - camY) $
                       color (makeColor 0 1 0 0.5) $
                       rectangleWire (2 * h) (2 * h)
        
        -- Factor de escala
        shapeScale :: Float
        shapeScale = tileSize / 32.0
        -- Convertir shape a coordenadas mundo (igual que Logic.hs)
        shapeToWorld :: Int -> Int -> CollisionShape -> CollisionShape
        shapeToWorld col row (CRect sx sy sw sh) =
            let sx' = sx * shapeScale
                sy' = sy * shapeScale
                sw' = sw * shapeScale
                sh' = sh * shapeScale
                tileX = fromIntegral col * tileSize
                tileY = fromIntegral (mapH - 1 - row) * tileSize
                worldX = tileX + sx'
                worldY = tileY + (tileSize - sy' - sh')
            in CRect worldX worldY sw' sh'
        shapeToWorld col row (CPoly pts) =
            let tileX = fromIntegral col * tileSize
                tileY = fromIntegral (mapH - 1 - row) * tileSize
                -- Los puntos en Tiled están en coordenadas del tile (0,0 = top-left)
                -- tileY es el BOTTOM del tile en Gloss
                worldPts = map (\(ptx, pty) -> 
                    let wx = tileX + ptx * shapeScale
                        wy = tileY + tileSize - pty * shapeScale
                    in (wx, wy)) pts
            in CPoly worldPts
        
        -- Dibujar un shape en coordenadas mundo
        drawShape :: CollisionShape -> Picture
        drawShape (CRect wx wy ww wh) =
            translate (wx + ww/2 - camX) (wy + wh/2 - camY) $
            color (makeColor 1 0 0 0.3) $
            rectangleSolid ww wh
        drawShape (CPoly pts) =
            let screenPts = map (\(wx, wy) -> (wx - camX, wy - camY)) pts
            in color (makeColor 1 0.5 0 0.4) $ polygon screenPts
        
        -- Rango visible
        visibleCols = [floor ((px - 300) / tileSize) .. ceiling ((px + 300) / tileSize)]
        visibleRows = [0 .. mapH - 1]
        
        normalizeGid gid = gid .&. 0x1FFFFFFF
        
        -- Dibujar todas las collision shapes visibles
        collisionPics = 
            [ drawShape worldShape
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
            , hasValidArea shape
            , let worldShape = shapeToWorld col row shape
            ]
        
        hasValidArea (CRect _ _ w h) = w > 0.5 && h > 0.5
        hasValidArea (CPoly pts) = length pts >= 3
        
    in pictures (playerBoxPic : collisionPics)


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


-- Renderizar proyectiles
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


-- Renderiza los items en el mundo con una animación de flote
renderWorldItems :: GameState -> Picture
renderWorldItems gs = 
    let cam = cameraPos (camera gs)
        camX = fst cam
        camY = snd cam
        items = worldItems gs

        p = player gs
        (px, py) = playerPos p
        
        renderItem item = 
            let (ix, iy) = itemPos item
                -- Animación de flote
                floatOffset = sin (itemFloatTime item * itemFloatSpeed) * itemFloatHeight

                screenX = ix - camX
                screenY = iy - camY + floatOffset

                -- Sprite del item
                itemPic = itemSprites Array.! itemType item

                -- Calcular la distancia al jugador
                dist = sqrt ((px - ix) ^ 2 + (py - iy) ^ 2)
                isNear = dist <= itemPickupRadius

                itemText = if isNear
                    then pictures
                        [   -- Nombre del item
                            translate screenX (screenY + 40) $
                            scale 0.12 0.12 $
                            color white $
                            text (itemName (itemType item))
                            , -- Instrucción
                            translate screenX (screenY + 20) $
                            scale 0.09 0.09 $
                            color (makeColor 0.9 0.9 0.9 1) $
                            text "[E] para recoger"
                        ]
                    else Blank
            
            in pictures
                [   translate screenX screenY $ scale 1.0 1.0 $ itemPic,
                    itemText
                ]
    
    in pictures (map renderItem items)

        