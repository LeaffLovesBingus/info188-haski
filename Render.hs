module Render where

import Assets
import Codec.Picture
import Codec.Picture.Types
import Control.Exception (IOException, try)
import Data.Array qualified as Array
import Data.Bits ((.&.), complement)
import Data.List (sortBy)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Graphics.Gloss
import Graphics.Gloss.Juicy
import HUD.Rendering (renderHUD)
import MapLoader (CollisionShape (..), TileLayer, TilesetInfo, loadMapFromJSON)
import System.FilePath (normalise)
import System.IO.Unsafe (unsafePerformIO)
import Types

-- Cache de capas
layersCache :: [TileLayer]
layersCache = unsafePerformIO $ do
  eres <- try (loadMapFromJSON "assets/map/mapa.JSON") :: IO (Either IOException ([TilesetInfo], [TileLayer], [[Bool]]))
  case eres of
    Right (_tilesets, layers, _coll) -> return layers
    Left _ -> return []
{-# NOINLINE layersCache #-}

-- Imagen de fondo del menú
menuBackgroundImage :: Maybe (Image PixelRGBA8)
menuBackgroundImage = unsafePerformIO $ do
  result <- readImage "assets/pantallas/inicio.png"
  return $ case result of
    Left _ -> Nothing
    Right dynImg -> Just $ convertRGBA8 dynImg
{-# NOINLINE menuBackgroundImage #-}

menuBackgroundPicture :: Picture
menuBackgroundPicture =
  case menuBackgroundImage of
    Just img -> fromImageRGBA8 img
    Nothing -> color (makeColor 0.1 0.15 0.2 1) $ rectangleSolid (fromIntegral screenWidth) (fromIntegral screenHeight)

-- Imagen de fondo de victoria
victoryBackgroundImage :: Maybe (Image PixelRGBA8)
victoryBackgroundImage = unsafePerformIO $ do
  result <- readImage "assets/pantallas/victoria.png"
  return $ case result of
    Left _ -> Nothing
    Right dynImg -> Just $ convertRGBA8 dynImg
{-# NOINLINE victoryBackgroundImage #-}

victoryBackgroundPicture :: Picture
victoryBackgroundPicture =
  case victoryBackgroundImage of
    Just img -> fromImageRGBA8 img
    Nothing -> color (makeColor 0.1 0.2 0.1 1) $ rectangleSolid (fromIntegral screenWidth) (fromIntegral screenHeight)

-- Imagen de fondo de derrota
defeatBackgroundImage :: Maybe (Image PixelRGBA8)
defeatBackgroundImage = unsafePerformIO $ do
  result <- readImage "assets/pantallas/derrota.png"
  return $ case result of
    Left _ -> Nothing
    Right dynImg -> Just $ convertRGBA8 dynImg
{-# NOINLINE defeatBackgroundImage #-}

defeatBackgroundPicture :: Picture
defeatBackgroundPicture =
  case defeatBackgroundImage of
    Just img -> fromImageRGBA8 img
    Nothing -> color (makeColor 0.2 0.1 0.1 1) $ rectangleSolid (fromIntegral screenWidth) (fromIntegral screenHeight)

tilesetBackground :: Maybe (Image PixelRGBA8)
tilesetBackground = unsafePerformIO $ do
  result <- readImage "assets/ambiente/pasto.png"
  return $ case result of
    Left _ -> Nothing
    Right dynImg -> Just $ convertRGBA8 dynImg
{-# NOINLINE tilesetBackground #-}

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

tileCache :: Array.Array Int Picture
tileCache = unsafePerformIO $ do
  let tiles = [extractTileIO i | i <- [0 .. 31]]
  Array.listArray (0, 31) <$> sequence tiles
{-# NOINLINE tileCache #-}

tilesetCache :: [(Int, [Picture], Int, Int, Int)]
tilesetCache = unsafePerformIO $ do
  eres <- try (loadMapFromJSON "assets/map/mapa.JSON") :: IO (Either IOException ([MapLoader.TilesetInfo], [MapLoader.TileLayer], [[Bool]]))
  case eres of
    Left _ -> return []
    Right (tilesetsInfo, _layers, _coll) -> do
      picsPer <-
        mapM
          ( \(fg, imgPath, tw, th, cols) -> do
              let p = normalise imgPath
              ps <- loadTileset p (if tw <= 0 then 32 else tw) (if th <= 0 then 32 else th)
              return (fg, ps, tw, th, cols)
          )
          tilesetsInfo
      return $ sortBy (comparing (\(fg, _, _, _, _) -> fg)) picsPer
{-# NOINLINE tilesetCache #-}

findTilesetForGid :: Int -> Maybe (Int, [Picture], Int, Int, Int)
findTilesetForGid gid
  | gid <= 0 = Nothing
  | otherwise =
      let applicable = filter (\(fg, _, _, _, _) -> fg <= gid) tilesetCache
       in if null applicable then Nothing else Just (last (sortBy (comparing (\(fg, _, _, _, _) -> fg)) applicable))

getTileFromGid :: Int -> Picture
getTileFromGid gid
  | gid <= 0 = Blank
  | otherwise =
      case findTilesetForGid gid of
        Nothing -> Blank
        Just (firstgid, pics, tw, th, cols) ->
          let idx = gid - firstgid
           in if idx >= 0 && idx < length pics
                then
                  let pic = pics !! idx
                   in let scaleFactor = tileSize / (fromIntegral (if tw <= 0 then 32 else tw))
                       in scale scaleFactor scaleFactor pic
                else Blank

extractTileIO :: Int -> IO Picture
extractTileIO tileIndex = return $ fromMaybe Blank $ do
  img <- tilesetBackground
  let tileRow = tileIndex `div` 8
      tileCol = tileIndex `mod` 8
      tileWidth = 32 :: Int
      tileHeight = 32 :: Int
      tilePic = extractRegion img (tileCol * tileWidth) (tileRow * tileHeight) tileWidth tileHeight
  fromImageRGBA8 <$> tilePic

extractRegion :: Image PixelRGBA8 -> Int -> Int -> Int -> Int -> Maybe (Image PixelRGBA8)
extractRegion img x y w h
  | x < 0 || y < 0 || x + w > imageWidth img || y + h > imageHeight img = Nothing
  | otherwise = Just $ generateImage getPixel w h
  where
    getPixel px py = pixelAt img (x + px) (y + py)

-- Renderizar juego completo
renderGame :: GameState -> Picture
renderGame gs = case currentScene gs of
  MenuScreen -> renderMenuScreen
  Playing    -> renderPlayingScreen gs
  Victory    -> renderVictoryScreen
  Defeat     -> renderDefeatScreen

-- Pantalla de menú principal
renderMenuScreen :: Picture
renderMenuScreen = pictures
  [ -- Fondo con imagen
    menuBackgroundPicture
  , -- Botón Jugar
    translate 0 20 $ renderButton "Jugar" (makeColor 0.2 0.6 0.3 1)
  , -- Botón Salir
    translate 0 (-80) $ renderButton "Salir" (makeColor 0.6 0.2 0.2 1)
  ]

-- Renderizar un botón
renderButton :: String -> Color -> Picture
renderButton label btnColor = 
  let textScale = 0.2
      -- Aproximación: cada carácter tiene ~60 unidades de ancho a escala 0.2
      charWidth = 60 * textScale
      textWidth = fromIntegral (length label) * charWidth
      textHeight = 100 * textScale  -- Altura aproximada del texto
      offsetX = -textWidth / 2
      offsetY = -textHeight / 2
  in pictures
    [ color btnColor $ rectangleSolid buttonWidth buttonHeight
    , color (makeColor 1 1 1 0.3) $ rectangleWire buttonWidth buttonHeight
    , translate offsetX offsetY $ scale textScale textScale $ color white $ text label
    ]

-- Constantes de botones
buttonWidth, buttonHeight :: Float
buttonWidth = 200
buttonHeight = 60

-- Posiciones Y de los botones (centro)
playButtonY, exitButtonY :: Float
playButtonY = 20
exitButtonY = -80

-- Pantalla de victoria
renderVictoryScreen :: Picture
renderVictoryScreen = pictures
  [ victoryBackgroundPicture
  , translate 0 menuButtonY $ renderButton "Menu" (makeColor 0.2 0.5 0.6 1)
  ]

-- Pantalla de derrota
renderDefeatScreen :: Picture
renderDefeatScreen = pictures
  [ defeatBackgroundPicture
  , translate 0 menuButtonY $ renderButton "Menu" (makeColor 0.2 0.5 0.6 1)
  ]

-- Posición Y del botón en pantallas de victoria/derrota
menuButtonY :: Float
menuButtonY = -280

-- Pantalla de juego
renderPlayingScreen :: GameState -> Picture
renderPlayingScreen gs =
  let cam = cameraPos (camera gs)
      camX = fst cam
      camY = snd cam

      -- Usar las capas almacenadas en el estado cuando estén disponibles,
      -- pero conservar los nombres de `layersCache` (que vienen del JSON/TSX).
      allLayersList =
        if null layersCache
          then [("legacy", tileMap gs)]
          else let names = map fst layersCache
                   tilesFromState = if null (allLayers gs) then map snd layersCache else allLayers gs
               in zip names tilesFromState

      (layersBelow, layersAbove) = splitAt 3 allLayersList

      renderLayers layers = pictures $ map (\(_, tiles) -> renderLayerTiles tiles camX camY) layers
   in pictures
        [ renderLayers layersBelow,      -- Capas 0,1,2 (debajo del jugador)
          renderPlayer gs,
          renderSwordSlash gs,
          renderEnemies gs,
          renderProjectiles gs,
          renderWorldItems gs,
          renderBoomerang gs,
          renderLayers layersAbove,      -- Capas 3,4 (Plantas, Props) - encima del jugador
          renderItemFlash gs,
          renderCursor gs,
          renderCooldownBar gs,
          renderDestructibleHealthBars gs,
          renderHUD gs
        --, renderDebugCollisions gs      -- Debug
        ]

-- Renderizar barras de vida sobre objetos dañados
renderDestructibleHealthBars :: GameState -> Picture
renderDestructibleHealthBars gs =
  let cam = cameraPos (camera gs)
      camX = fst cam
      camY = snd cam
      objs = destructibleObjects gs
      
      renderBar obj =
        let (ox, oy) = destPos obj
            screenX = ox - camX
            screenY = oy - camY
            
            -- Solo mostrar si está dañado
            isDamaged = destHealth obj < destMaxHealth obj
            
            barWidth = 50.0
            barHeight = 6.0
            barY = screenY + 35
            
            healthPercent = destHealth obj / destMaxHealth obj
            fillWidth = barWidth * healthPercent
            
            -- Color según vida
            barColor = if healthPercent > 0.6
                       then makeColor 0.2 1.0 0.2 0.9  -- Verde
                       else if healthPercent > 0.3
                       then makeColor 1.0 0.9 0.0 0.9  -- Amarillo
                       else makeColor 1.0 0.2 0.2 0.9  -- Rojo
            
            background = translate screenX barY $
                color (makeColor 0.1 0.1 0.1 0.8) $
                rectangleSolid barWidth barHeight
            
            fill = translate (screenX - barWidth/2 + fillWidth/2) barY $
                color barColor $
                rectangleSolid fillWidth barHeight
            
            border = translate screenX barY $
                color white $
                rectangleWire barWidth barHeight
        in if isDamaged
           then pictures [background, fill, border]
           else Blank
  in pictures (map renderBar objs)

renderLayerTiles :: [[Int]] -> Float -> Float -> Picture
renderLayerTiles tiles camX camY =
  let screenWidthF = fromIntegral screenWidth
      screenHeightF = fromIntegral screenHeight
      mapH = length tiles

      leftWorld = camX - screenWidthF / 2
      rightWorld = camX + screenWidthF / 2
      topWorld = camY + screenHeightF / 2
      bottomWorld = camY - screenHeightF / 2

      worldYToRow wy = mapH - 1 - floor (wy / tileSize)

      minTileX = floor (leftWorld / tileSize) - 1
      maxTileX = ceiling (rightWorld / tileSize) + 1

      r1 = worldYToRow topWorld
      r2 = worldYToRow bottomWorld
      minTileY = max 0 (min r1 r2 - 1)
      maxTileY = min (max 0 (mapH - 1)) (max r1 r2 + 1)

      visibleTiles =
        [ renderTile tiles x y camX camY
          | x <- [minTileX .. maxTileX],
            y <- [minTileY .. maxTileY]
        ]
   in pictures visibleTiles

renderTile :: [[Int]] -> Int -> Int -> Float -> Float -> Picture
renderTile tiles x y camX camY =
  let mapH = length tiles
      worldX = fromIntegral x * tileSize + tileSize / 2
      worldY = fromIntegral (mapH - 1 - y) * tileSize + tileSize / 2
      screenX = worldX - camX
      screenY = worldY - camY
      rawGid =
        if y >= 0 && y < mapH && x >= 0 && (not (null tiles)) && x < length (head tiles)
          then tiles !! y !! x
          else -1
      
      -- Flags de flip de Tiled
      flipHorizontal = (rawGid .&. 0x80000000) /= 0
      flipVertical   = (rawGid .&. 0x40000000) /= 0
      flipDiagonal   = (rawGid .&. 0x20000000) /= 0
      
      -- GID limpio sin flags
      tileIndex = rawGid .&. 0x1FFFFFFF
      
      basePic = if tileIndex <= 0 then Blank else getTileFromGid tileIndex
      
      -- Aplicar transformaciones
      flippedPic = applyFlips basePic flipHorizontal flipVertical flipDiagonal
      
   in translate screenX screenY flippedPic

-- Aplicar flags de flip a una imagen
applyFlips :: Picture -> Bool -> Bool -> Bool -> Picture
applyFlips pic flipH flipV flipD
  | flipD && flipH && flipV = rotate 90 $ scale 1 (-1) pic   -- Rotacion 90 + flip
  | flipD && flipH          = rotate 90 pic                   -- Rotacion 90
  | flipD && flipV          = rotate (-90) pic                -- Rotacion -90
  | flipD                   = rotate 90 $ scale (-1) 1 pic    -- Rotacion 90 + flip horizontal
  | flipH && flipV          = scale (-1) (-1) pic             -- Flip ambos
  | flipH                   = scale (-1) 1 pic                -- Flip horizontal
  | flipV                   = scale 1 (-1) pic                -- Flip vertical
  | otherwise               = pic


-- Renderiza al jugador
-- Escoge de forma dinámica la sprite sheet a usar dependiendo del arma equipada del jugador
renderPlayer :: GameState -> Picture
renderPlayer gs =
  let p = player gs
      cam = cameraPos (camera gs)
      (px, py) = playerPos p
      screenX = px - fst cam
      screenY = py - snd cam

      -- Seleccionar animación del jugador
      (vx, vy) = playerVel p
      isMoving = vx /= 0 || vy /= 0
      anim = if playerIsTakingDamage p
             then Damage 
             else if isMoving
              then Walk
              else Idle

      -- Determinar rangos de frames para cada tipo de animación
      dir = playerDir p
      frame = playerFrame p
      maxFrame = case anim of
        Idle -> 1
        Walk -> 3
        Damage -> 3
      safeFrame = frame `mod` (maxFrame + 1)

      -- Seleccionar el array de frames correcto según el arma equipada
      framesArray = case playerEquippedItem p of
        Just Ballesta -> playerFramesBallesta
        Just Espada -> playerFramesEspada
        Just Boomerang ->
          if playerHasBoomerang p
          then playerFramesBoomerang
          else playerFrames
        _ -> playerFrames

      framePic = framesArray Array.! (dir, anim, safeFrame)
   in translate screenX screenY $
        scale 2.0 2.0 $
          framePic

renderCooldownBar :: GameState -> Picture
renderCooldownBar gs =
  let p = player gs
      cam = cameraPos (camera gs)
      (px, py) = playerPos p
      screenX = px - fst cam
      screenY = py - snd cam
      currentCD = playerCooldownBallesta p
      hasBallesta = case playerEquippedItem p of
        Just Ballesta -> True
        _ -> False
      shouldShow = hasBallesta && currentCD > 0.0
   in if shouldShow
        then
          let progress = currentCD / cooldownBallesta
              fillWidth = cooldownBarWidth * (1.0 - progress)
              barY = screenY + 35
              background =
                translate screenX barY $
                  color (makeColor 0.2 0.2 0.2 0.6) $
                    rectangleSolid cooldownBarWidth cooldownBarHeight
              progressBar =
                translate (screenX - cooldownBarWidth / 2 + fillWidth / 2) barY $
                  color (makeColor 1.0 1.0 1.0 0.6) $
                    rectangleSolid fillWidth cooldownBarHeight
           in pictures [background, progressBar]
        else Blank

renderProjectiles :: GameState -> Picture
renderProjectiles gs =
  let cam = cameraPos (camera gs)
      camX = fst cam
      camY = snd cam
      projs = projectiles gs
      renderProj proj =
        let (px, py) = projPos proj
            (vx, vy) = projVel proj
            screenX = px - camX
            screenY = py - camY
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
   in translate mx my $
        scale 0.3 0.3 $
          crosshairPicture

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
            floatOffset = sin (itemFloatTime item * itemFloatSpeed) * itemFloatHeight
            screenX = ix - camX
            screenY = iy - camY + floatOffset
            itemPic = itemSprites Array.! itemType item
            dist = sqrt ((px - ix) ^ 2 + (py - iy) ^ 2)
            isNear = dist <= itemPickupRadius
            itemText =
              if isNear
                then
                  pictures
                    [ translate screenX (screenY + 40) $
                        scale 0.12 0.12 $
                          color white $
                            text (itemName (itemType item)),
                      translate screenX (screenY + 20) $
                        scale 0.09 0.09 $
                          color (makeColor 0.9 0.9 0.9 1) $
                            text "[E] para recoger"
                    ]
                else Blank
         in pictures
              [ translate screenX screenY $ scale 1.0 1.0 $ itemPic,
                itemText
              ]
  in pictures (map renderItem items)


-- Renderizar el boomerang
renderBoomerang :: GameState -> Picture
renderBoomerang gs = 
  case boomerang gs of
    Nothing -> Blank
    Just b ->
      let cam = cameraPos (camera gs)
          camX = fst cam
          camY = snd cam
          (bx, by) = boomerangPos b
          rotation = boomerangRotation b 
          screenX = bx - camX
          screenY = by - camY
      in translate screenX screenY $
        rotate rotation $
        scale 1.0 1.0 $
        boomerangProjectilePicture


-- Renderizar el flash del nombre del item sobre el jugador
renderItemFlash :: GameState -> Picture
renderItemFlash gs =
  let p = player gs
      flashState = playerItemFlashState p
      cam = cameraPos (camera gs)
      (px, py) = playerPos p
      screenX = px - fst cam
      screenY = py - snd cam
  in case flashState of
      NoFlash -> Blank
      
      Showing iType ->
        translate screenX (screenY + itemNameFlashYOffset) $
          scale 0.12 0.12 $
            color white $
              text (itemName iType)
      
      FadingOut iType fadeTime ->
        let alpha = playerItemFlashTimer p / itemNameFadeOutDuration
            fadeColor = makeColor 1.0 1.0 1.0 alpha
        in translate screenX (screenY + itemNameFlashYOffset) $
            scale 0.12 0.12 $
              color fadeColor $
                text (itemName iType)

-- Renderizar el slash de espada
renderSwordSlash :: GameState -> Picture
renderSwordSlash gs =
  case swordSlash gs of
    Nothing -> Blank
    Just slash ->
      let cam = cameraPos (camera gs)
          camX = fst cam
          camY = snd cam
          (sx, sy) = slashPos slash
          angle = slashAngle slash
          frame = slashFrame slash
          
          screenX = sx - camX
          screenY = sy - camY
          
          framePic = if frame >= 0 && frame < length slashFrames
                     then slashFrames !! frame
                     else Blank
      in translate screenX screenY $
          rotate (-angle) $
          scale 0.3 0.3 $
          framePic

-- Renderizar enemigos
renderEnemies :: GameState -> Picture
renderEnemies gs =
    let cam = cameraPos (camera gs)
        camX = fst cam
        camY = snd cam
        enemyList = Map.elems (enemies gs)
        
        renderEnemy enemy =
            let (ex, ey) = position enemy
                screenX = ex - camX
                screenY = ey - camY
                
                -- Obtener frame actual de la animación
                frame = enemyFrame enemy
                safeFrame = frame `mod` 2  -- Asegurar que esté entre 0 y 1
                
                -- Obtener sprite del frame
                enemyPic = enemyFrames Array.! safeFrame
                
                -- Escala del sprite
                spriteScale = 1.3
                
            in translate screenX screenY $
                scale spriteScale spriteScale $
                    enemyPic
                    
    in pictures (map renderEnemy enemyList)