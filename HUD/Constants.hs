module HUD.Constants (hudColor, hudDarkColor, healthBarColorBottom, healthBarColorTop, barWidthMax, barHeight, borderRadius)  where

import Graphics.Gloss

-- Colores

hudColor :: Color
hudColor = white

hudDarkColor :: Color -- #c63737
hudDarkColor = makeColor 0.776 0.216 0.216 1.0

healthBarColorBottom :: Color -- #ac3232
healthBarColorBottom = makeColor 0.6745 0.1961 0.1961 1.0

healthBarColorTop :: Color -- #d03c3c
healthBarColorTop = makeColor 0.8157 0.2353 0.2353 1.0

-- Constantes

barWidthMax :: Float
barWidthMax = 200.0

barHeight :: Float
barHeight = 20.0

borderRadius :: Float
borderRadius = barHeight / 2.0