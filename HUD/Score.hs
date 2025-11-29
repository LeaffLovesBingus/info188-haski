module HUD.Score (drawScore) where

import Graphics.Gloss
import Types
import HUD.Constants (hudColor)

drawScore :: Int -> Picture
drawScore score =
  let screenW = fromIntegral screenWidth
      screenH = fromIntegral screenHeight
   in color hudColor $
        scale 0.15 0.15 $
          text ("SCORE: " ++ show score)