module HUD.Rendering (renderHUD) where

import Assets (heartImage, itemSprites)
import Data.Array ((!))
import Data.Maybe (fromMaybe)
import Graphics.Gloss
import Types
import HUD.Constants
import HUD.HealthBar
import HUD.Inventory (drawInventory)


-- Render HUD completo

renderHUD :: GameState -> Picture
renderHUD gs =
  let screenW = fromIntegral screenWidth
      screenH = fromIntegral screenHeight
      hpBarX = - (screenW / 2) + 100.0 + 50.0
      hpBarY = - (screenH / 2) + 60.0

      scoreX = screenW / 2 - 150.0
      scoreY = screenH / 2 - 50.0

      itemX = screenW / 2 - 60.0
      itemY = - (screenH / 2) + 60.0
      
      -- Posicion del temporizador (arriba centro)
      timerX = 0
      timerY = (screenH / 2) - 50.0
   in pictures
        [ translate (hpBarX - 120) hpBarY heartImage,
          translate hpBarX hpBarY $ drawHealthBar (playerHealth (player gs)),
          drawInventory (player gs),
          translate timerX timerY $ drawTimer (gameTimer gs)
        ]

-- Dibujar el temporizador
drawTimer :: Float -> Picture
drawTimer timeLeft =
  let minutes = floor (timeLeft / 60) :: Int
      seconds = floor timeLeft `mod` 60 :: Int
      timeStr = padZero minutes ++ ":" ++ padZero seconds
      
      -- Color segun el tiempo restante
      timerColor = if timeLeft <= 30 
                   then makeColor 1 0.2 0.2 1  -- Rojo cuando quedan 30s
                   else if timeLeft <= 60
                   then makeColor 1 0.8 0.2 1  -- Amarillo cuando queda 1 min
                   else white                   -- Blanco normalmente
      
      -- Fondo del temporizador
      bgWidth = 85
      bgHeight = 36
      background = color (makeColor 0 0 0 0.75) $ rectangleSolid bgWidth bgHeight
      
      -- Texto del tiempo centrado
      timerText = translate (-32) (-9) $ scale 0.18 0.18 $ color timerColor $ text timeStr
      
  in pictures [background, timerText]

-- Agregar cero a la izquierda si es necesario
padZero :: Int -> String
padZero n = if n < 10 then "0" ++ show n else show n
