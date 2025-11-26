module Render where

import Types
import Assets
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Codec.Picture
import Codec.Picture.Types
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromMaybe)
import qualified Data.Array as Array


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


-- Cache de tiles pre-renderizados
tileCache :: Array.Array Int Picture
tileCache = unsafePerformIO $ do
    let tiles = [extractTileIO i | i <- [0..31]]
    Array.listArray(0, 31) <$> sequence tiles
{-# NOINLINE tileCache #-}


-- Extraer un tile específico del tileset
extractTileIO :: Int -> IO Picture
extractTileIO tileIndex = return $ fromMaybe Blank $ do
    img <- tilesetBackground
    let tileRow = tileIndex `div` 8
        tileCol = tileIndex `mod` 8

        tileWidth = 32 :: Int
        tileHeight = 32 :: Int

        tilePic = extractRegion img (tileCol * tileWidth) (tileRow * tileHeight) tileWidth tileHeight
    
    fromImageRGBA8 <$> tilePic


-- Devuelve un tile del cache
getTile :: Int -> Picture
getTile idx =
    if idx >= 0 && idx <= 31
    then tileCache Array.! idx
    else Blank


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
    ]


-- Renderizar el fondo con tiles
renderBackground :: GameState -> Picture
renderBackground gs =
    let cam = cameraPos (camera gs)
        camX = fst cam
        camY = snd cam

        -- Calcular las tiles visibles
        screenWidthF = fromIntegral screenWidth
        screenHeightF = fromIntegral screenHeight

        minTileX = floor ((camX - screenWidthF/2) / tileSize) - 1
        maxTileX = ceiling ((camX + screenWidthF/2) / tileSize) + 1
        minTileY = floor ((camY - screenHeightF/2) / tileSize) - 1
        maxTileY = ceiling ((camY + screenHeightF/2) / tileSize) + 1

        tiles = tileMap gs

        visibleTiles = [renderTile tiles x y camX camY
                        | x <- [minTileX..maxTileX],
                          y <- [minTileY..maxTileY]]
    in pictures visibleTiles


-- Renderiza un tile individual
renderTile :: [[Int]] -> Int -> Int -> Float -> Float -> Picture
renderTile tiles x y camX camY = 
    let worldX = fromIntegral x * tileSize
        worldY = fromIntegral y * tileSize

        -- Posición en pantalla relativa a la cámara
        screenX = worldX - camX
        screenY = worldY - camY

        -- Obtener índice del tile 
        tileIndex = if y >= 0 && y < length tiles && x >= 0 && x < length (tiles !! 0)
                  then tiles !! y !! x
                  else -1

        -- Obtener el tile del tilesheet
        tilePic = 
            if tileIndex == -1
                then Blank
                else getTile tileIndex

    in translate screenX screenY
        $ scale (tileSize / 32.0) (tileSize / 32.0)
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
        $ scale 3.0 3.0
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