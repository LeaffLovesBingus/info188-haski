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
   in pictures
        [ translate (hpBarX - 120) hpBarY heartImage,
          translate hpBarX hpBarY $ drawHealthBar (playerHealth (player gs)),
          drawInventory (player gs)
        ]
