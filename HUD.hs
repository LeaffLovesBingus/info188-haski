module HUD (renderHUD) where

import Assets (heartImage)
import Data.Maybe (fromMaybe)
import Graphics.Gloss
import Types

-- Colores

hudColor :: Color
hudColor = white

hudDarkColor :: Color -- #c63737
hudDarkColor = makeColor 0.776 0.216 0.216 1.0

colorBottom :: Color -- #ac3232
colorBottom = makeColor 0.6745 0.1961 0.1961 1.0

colorTop :: Color -- #d03c3c
colorTop = makeColor 0.8157 0.2353 0.2353 1.0

-- Constantes

barWidthMax :: Float
barWidthMax = 200.0

barHeight :: Float
barHeight = 20.0

borderRadius :: Float
borderRadius = barHeight / 2.0

-- Render HUD completo

renderHUD :: GameState -> Picture
renderHUD gs =
  let screenW = fromIntegral screenWidth
      screenH = fromIntegral screenHeight
      hpBarX = -screenW / 2 + 100.0 + 50.0
      hpBarY = -screenH / 2 + 80.0

      -- Posición para el score (asumiendo top-right como el original)
      scoreX = screenW / 2 - 150.0
      scoreY = screenH / 2 - 50.0
   in pictures
        [ translate (hpBarX - 120) hpBarY heartImage,
          translate hpBarX hpBarY $ drawRoundedBar (playerHealth (player gs)),
          translate scoreX scoreY $ drawScore 0
        ]

-- Barra de vida
drawRoundedBar :: Int -> Picture
drawRoundedBar hp =
  pictures [drawOutline, drawFill]
  where
    currentHp = hp -- el HP pasado como argumento
    healthColor = colorTop
    -- La barra tiene un ancho de 200 para 100 HP, por lo que el factor es 2.0.
    currentWidth = min barWidthMax (2.0 * fromIntegral currentHp)

    outlineWidth = barWidthMax + 4
    outlineHeight = barHeight + 4
    outlineRadius = borderRadius + 2
    -- Contorno blanco
    drawOutline =
      pictures
        -- Rectángulo Central
        [ color white $
            rectangleSolid (outlineWidth - 2 * outlineRadius) outlineHeight,
          -- Círculo Izquierdo
          translate (-(outlineWidth / 2) + outlineRadius) 0 $
            color white $
              circleSolid outlineRadius,
          -- Círculo Derecho
          translate ((outlineWidth / 2) - outlineRadius) 0 $
            color white $
              circleSolid outlineRadius
        ]
    -- Relleno de color (simulación de rectángulo redondeado anclado a la izquierda)
    drawFill =
      if currentHp <= 0
        then Blank
        else
          let baseX = -(barWidthMax / 2) -- Borde izquierdo de la barra
              fullFillWidth = currentWidth
              rectW = max 0 (fullFillWidth - 2 * borderRadius)

              -- Centro del rectángulo central
              centerRectX = baseX + borderRadius + rectW / 2

              -- Centro del círculo derecho (se mueve con el ancho de la barra)
              centerRightCircleX = baseX + fullFillWidth - borderRadius
           in pictures
                [ -- Círculo izquierdo (anclado al inicio de la barra)
                  translate (baseX + borderRadius) 0 $
                    color healthColor $
                      circleSolid borderRadius,
                  -- Rectángulo central
                  translate centerRectX 0 $
                    color healthColor $
                      rectangleSolid rectW barHeight,
                  if fullFillWidth > borderRadius
                    then
                      translate centerRightCircleX 0 $
                        color healthColor $
                          circleSolid borderRadius
                    else Blank
                ]

-- SCORE

drawScore :: Int -> Picture
drawScore score =
  let screenW = fromIntegral screenWidth
      screenH = fromIntegral screenHeight
   in color hudColor $
        scale 0.15 0.15 $
          text ("SCORE: " ++ show score)
