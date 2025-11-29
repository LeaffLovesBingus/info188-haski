module HUD.HealthBar (drawHealthBar) where

import Graphics.Gloss
import HUD.Constants (barWidthMax, barHeight, borderRadius, healthBarColorTop, healthBarColorBottom)

-- Barra de vida
drawHealthBar :: Int -> Picture
drawHealthBar hp =
  pictures [drawOutline, drawFill]
  where
    currentHp = hp -- el HP pasado como argumento
    healthColor = healthBarColorTop
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