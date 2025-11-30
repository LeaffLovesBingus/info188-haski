module Assets where

import Codec.Picture
import Codec.Picture.Types
import Data.Array qualified as Array
import Data.Maybe (fromMaybe)
import Graphics.Gloss
import Graphics.Gloss.Juicy
import System.IO.Unsafe (unsafePerformIO)
import Types


------------------- ITEMS -------------------
-- Carga los sprites de los items
itemSprites :: Array.Array ItemType Picture
itemSprites = unsafePerformIO $ do
  ballesta <- loadItemSprite "assets/items/armas/ballesta.png"
  boomerang <- loadItemSprite "assets/items/armas/boomerang.png"
  espada <- loadItemSprite "assets/items/armas/espada.png"
  curacion <- loadItemSprite "assets/items/efectos/pocion_curacion.png"
  fuerza <- loadItemSprite "assets/items/efectos/pocion_fuerza.png"
  velocidad <- loadItemSprite "assets/items/efectos/pocion_velocidad.png"
  stamina <- loadItemSprite "assets/items/efectos/pocion_stamina.png"

  return $
    Array.array
      (Ballesta, Fuerza)
      [ (Ballesta, ballesta),
        (Boomerang, boomerang),
        (Espada, espada),
        (Curacion, curacion),
        (Velocidad, velocidad),
        (Fuerza, fuerza),
        (Stamina, stamina)
      ]
{-# NOINLINE itemSprites #-}

loadItemSprite :: FilePath -> IO Picture
loadItemSprite path = do
  result <- readImage path
  case result of
    Left _ -> return Blank
    Right dyn -> return $ fromImageRGBA8 (convertRGBA8 dyn)


------------------- ARMAS -------------------
-- Cargar el asset de boomerang como proyectil
boomerangProjectileImage :: Maybe (Image PixelRGBA8)
boomerangProjectileImage = unsafePerformIO $ do
    result <- readImage "assets/items/armas/boomerang.png"
    return $ case result of 
        Left _ -> Nothing
        Right dynImg -> Just (convertRGBA8 dynImg)
{-# NOINLINE boomerangProjectileImage #-}


boomerangProjectilePicture :: Picture
boomerangProjectilePicture =
    case boomerangProjectileImage of
        Just img -> fromImageRGBA8 img
        Nothing -> color blue (circleSolid 8) -- Por defecto un círculo azul


-- Cargar frames del slash de espada
slashFrames :: [Picture]
slashFrames = unsafePerformIO $ do
  frames <- mapM loadSlashFrame [1..6]
  return frames
  where
    loadSlashFrame :: Int -> IO Picture
    loadSlashFrame frameNum = do
      let path = "assets/items/slash/" ++ show frameNum ++ ".png"
      result <- readImage path
      case result of
        Left _ -> return Blank
        Right dynImg -> return $ fromImageRGBA8 (convertRGBA8 dynImg)
{-# NOINLINE slashFrames #-}


------------------- TILEMAP-------------------
-- Nueva: cargar un tileset (.png) y devolver la lista de tiles (Pictures).
-- No sustituye nada existente: es una función utilitaria adicional.
loadTileset :: FilePath -> Int -> Int -> IO [Picture]
loadTileset path tileW tileH = do
  eimg <- readImage path
  case eimg of
    Left _ -> return []
    Right dyn -> do
      let img = convertRGBA8 dyn
          iw = imageWidth img
          ih = imageHeight img
          cols = iw `div` tileW
          rows = ih `div` tileH
          makeTile col row =
            let cropped = generateImage (\x y -> pixelAt img (col * tileW + x) (row * tileH + y)) tileW tileH
             in fromImageRGBA8 (cropped :: Image PixelRGBA8)
          pics = [makeTile c r | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
      return pics


------------------- HUD -------------------
heartImage :: Picture
heartImage = unsafePerformIO $ do
  result <- readImage "assets/hud/heart.png"
  case result of
    Left errorMsg -> do
      putStrLn $ "Error al cargar assets/hud/heart.png: " ++ errorMsg
      return $ color red $ rectangleSolid 20 20 -- Placeholder rojo si falla la carga
    Right dynImg -> do
      let rgbaImg = convertRGBA8 dynImg
      return $ fromImageRGBA8 rgbaImg
{-# NOINLINE heartImage #-}


------------------- JUGADOR -------------------
-- Carga la spritesheet del jugador
playerSpriteSheet :: Maybe (Image PixelRGBA8)
playerSpriteSheet = unsafePerformIO $ do
  img <- readImage "assets/entidades/jugador.png"
  case img of
    Left _ -> return Nothing
    Right dyn -> return (Just (convertRGBA8 dyn))
{-# NOINLINE playerSpriteSheet #-}

-- Carga la spritesheet del jugador con ballesta
playerBallestaSpriteSheet :: Maybe (Image PixelRGBA8)
playerBallestaSpriteSheet = unsafePerformIO $ do
  img <- readImage "assets/entidades/jugador_ballesta.png"
  case img of
    Left _ -> return Nothing
    Right dyn -> return (Just (convertRGBA8 dyn))
{-# NOINLINE playerBallestaSpriteSheet #-}

-- Carga la spritesheet del jugador con boomerang
playerBoomerangSpriteSheet :: Maybe (Image PixelRGBA8)
playerBoomerangSpriteSheet = unsafePerformIO $ do
  img <- readImage "assets/entidades/jugador_boomerang.png"
  case img of
    Left _ -> return Nothing
    Right dyn -> return (Just (convertRGBA8 dyn))
{-# NOINLINE playerBoomerangSpriteSheet #-}

-- Carga la spritesheet del jugador con espada
playerEspadaSpriteSheet :: Maybe (Image PixelRGBA8)
playerEspadaSpriteSheet = unsafePerformIO $ do
  img <- readImage "assets/entidades/jugador_espada.png"
  case img of
    Left _ -> return Nothing
    Right dyn -> return (Just (convertRGBA8 dyn))
{-# NOINLINE playerEspadaSpriteSheet #-}

-- Extraer un frame en específico de una spritesheet dada
extractFrameFrom :: Maybe (Image PixelRGBA8) -> Int -> Int -> Int -> Int -> Maybe (Image PixelRGBA8)
extractFrameFrom sheetMaybe col row w h = do
  sheet <- sheetMaybe
  let sx = col * w
      sy = row * h
  if sx + w > imageWidth sheet || sy + h > imageHeight sheet
    then Nothing
    else Just $ generateImage (\px py -> pixelAt sheet (sx + px) (sy + py)) w h

-- Extraer un frame en específico (usa la spritesheet normal por defecto)
extractFrame :: Int -> Int -> Int -> Int -> Maybe (Image PixelRGBA8)
extractFrame = extractFrameFrom playerSpriteSheet

playerFrames :: Array.Array (Direction, AnimType, Int) Picture
playerFrames = unsafePerformIO $ do
  let w = 32
      h = 32

      -- filas del spritesheet
      rowIdleDown = 0
      rowIdleRight = 1
      rowIdleUp = 2
      rowWalkDown = 3
      rowWalkRight = 4
      rowWalkUp = 5
      rowDamageDown = 6
      rowDamageRight = 7
      rowDamageUp = 8

      extract col row =
        fromMaybe Blank (fromImageRGBA8 <$> extractFrame col row w h)

      idleFramesDown = [extract 0 rowIdleDown, extract 1 rowIdleDown]
      idleFramesRight = [extract 0 rowIdleRight, extract 1 rowIdleRight]
      idleFramesUp = [extract 0 rowIdleUp, extract 1 rowIdleUp]

      walkFramesDown = [extract i rowWalkDown | i <- [0 .. 3]]
      walkFramesRight = [extract i rowWalkRight | i <- [0 .. 3]]
      walkFramesUp = [extract i rowWalkUp | i <- [0 .. 3]]

      damageFramesDown = [extract i rowDamageDown | i <- [0 .. 3]]
      damageFramesRight = [extract i rowDamageRight | i <- [0 .. 3]]
      damageFramesUp = [extract i rowDamageUp | i <- [0 .. 3]]

      -- izquierda = invertir derecha
      flipPic p = scale (-1) 1 p

      idleFramesLeft = map flipPic idleFramesRight
      walkFramesLeft = map flipPic walkFramesRight
      damageFramesLeft = map flipPic damageFramesRight

      list =
        -- Idle
        [((DirDown, Idle, i), idleFramesDown !! i) | i <- [0 .. 1]]
          ++ [((DirRight, Idle, i), idleFramesRight !! i) | i <- [0 .. 1]]
          ++ [((DirUp, Idle, i), idleFramesUp !! i) | i <- [0 .. 1]]
          ++ [((DirLeft, Idle, i), idleFramesLeft !! i) | i <- [0 .. 1]]
          -- Walk
          ++ [((DirDown, Walk, i), walkFramesDown !! i) | i <- [0 .. 3]]
          ++ [((DirRight, Walk, i), walkFramesRight !! i) | i <- [0 .. 3]]
          ++ [((DirUp, Walk, i), walkFramesUp !! i) | i <- [0 .. 3]]
          ++ [((DirLeft, Walk, i), walkFramesLeft !! i) | i <- [0 .. 3]]
          -- Damage
          ++ [((DirDown, Damage, i), damageFramesDown !! i) | i <- [0 .. 3]]
          ++ [((DirRight, Damage, i), damageFramesRight !! i) | i <- [0 .. 3]]
          ++ [((DirUp, Damage, i), damageFramesUp !! i) | i <- [0 .. 3]]
          ++ [((DirLeft, Damage, i), damageFramesLeft !! i) | i <- [0 .. 3]]

  return (Array.array ((DirDown, Idle, 0), (DirLeft, Damage, 3)) list)
{-# NOINLINE playerFrames #-}


-- Función auxiliar para generar frames desde una spritesheet específica
generatePlayerFrames :: Maybe (Image PixelRGBA8) -> Array.Array (Direction, AnimType, Int) Picture
generatePlayerFrames spriteSheet = unsafePerformIO $ do
  let w = 32
      h = 32

      -- filas del spritesheet
      rowIdleDown = 0
      rowIdleRight = 1
      rowIdleUp = 2
      rowWalkDown = 3
      rowWalkRight = 4
      rowWalkUp = 5
      rowDamageDown = 6
      rowDamageRight = 7
      rowDamageUp = 8

      extract col row =
        fromMaybe Blank (fromImageRGBA8 <$> extractFrameFrom spriteSheet col row w h)

      idleFramesDown = [extract 0 rowIdleDown, extract 1 rowIdleDown]
      idleFramesRight = [extract 0 rowIdleRight, extract 1 rowIdleRight]
      idleFramesUp = [extract 0 rowIdleUp, extract 1 rowIdleUp]

      walkFramesDown = [extract i rowWalkDown | i <- [0 .. 3]]
      walkFramesRight = [extract i rowWalkRight | i <- [0 .. 3]]
      walkFramesUp = [extract i rowWalkUp | i <- [0 .. 3]]

      damageFramesDown = [extract i rowDamageDown | i <- [0 .. 3]]
      damageFramesRight = [extract i rowDamageRight | i <- [0 .. 3]]
      damageFramesUp = [extract i rowDamageUp | i <- [0 .. 3]]

      -- izquierda = invertir derecha
      flipPic p = scale (-1) 1 p

      idleFramesLeft = map flipPic idleFramesRight
      walkFramesLeft = map flipPic walkFramesRight
      damageFramesLeft = map flipPic damageFramesRight

      list =
        -- Idle
        [((DirDown, Idle, i), idleFramesDown !! i) | i <- [0 .. 1]]
          ++ [((DirRight, Idle, i), idleFramesRight !! i) | i <- [0 .. 1]]
          ++ [((DirUp, Idle, i), idleFramesUp !! i) | i <- [0 .. 1]]
          ++ [((DirLeft, Idle, i), idleFramesLeft !! i) | i <- [0 .. 1]]
          -- Walk
          ++ [((DirDown, Walk, i), walkFramesDown !! i) | i <- [0 .. 3]]
          ++ [((DirRight, Walk, i), walkFramesRight !! i) | i <- [0 .. 3]]
          ++ [((DirUp, Walk, i), walkFramesUp !! i) | i <- [0 .. 3]]
          ++ [((DirLeft, Walk, i), walkFramesLeft !! i) | i <- [0 .. 3]]
          -- Damage
          ++ [((DirDown, Damage, i), damageFramesDown !! i) | i <- [0 .. 3]]
          ++ [((DirRight, Damage, i), damageFramesRight !! i) | i <- [0 .. 3]]
          ++ [((DirUp, Damage, i), damageFramesUp !! i) | i <- [0 .. 3]]
          ++ [((DirLeft, Damage, i), damageFramesLeft !! i) | i <- [0 .. 3]]

  return (Array.array ((DirDown, Idle, 0), (DirLeft, Damage, 3)) list)


-- Frames para cada tipo de arma
playerFramesBallesta :: Array.Array (Direction, AnimType, Int) Picture
playerFramesBallesta = unsafePerformIO $ return $ generatePlayerFrames playerBallestaSpriteSheet
{-# NOINLINE playerFramesBallesta #-}

playerFramesBoomerang :: Array.Array (Direction, AnimType, Int) Picture
playerFramesBoomerang = unsafePerformIO $ return $ generatePlayerFrames playerBoomerangSpriteSheet
{-# NOINLINE playerFramesBoomerang #-}

playerFramesEspada :: Array.Array (Direction, AnimType, Int) Picture
playerFramesEspada = unsafePerformIO $ return $ generatePlayerFrames playerEspadaSpriteSheet
{-# NOINLINE playerFramesEspada #-}
