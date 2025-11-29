module HUD.Inventory (drawInventory) where

import Graphics.Gloss
import Types
import Assets (itemSprites)
import Data.Array ((!))
import HUD.Constants (hudDarkColor)

-- Configuración visual
slotSize :: Float
slotSize = 50.0

slotPadding :: Float
slotPadding = 10.0

slotBgColor :: Color
slotBgColor = makeColor 0.1 0.1 0.1 0.5 -- Fondo semitransparente oscuro

borderColor :: Color
borderColor = makeColor 0.8 0.8 0.8 0.8 -- Borde gris claro

selectedColor :: Color
selectedColor = white -- Color para el slot seleccionado

-- Dibujar todo el inventario
drawInventory :: Player -> Picture
drawInventory p =
  let inv = playerInventory p
      sel = playerSelectedSlot p
      
      -- Centrar abajo
      totalWidth = (slotSize * fromIntegral inventorySize) + (slotPadding * fromIntegral (inventorySize - 1))
      startX = -(totalWidth / 2) + (slotSize / 2)
      screenH = fromIntegral screenHeight
      yPos = -(screenH / 2) + 60.0 -- 60 pixeles desde el borde inferior
      
  in pictures [ drawSlot i item (i == sel) startX yPos | (i, item) <- zip [0..] inv ]

-- Dibujar 1 slot
drawSlot :: Int -> Maybe ItemType -> Bool -> Float -> Float -> Picture
drawSlot index item isSelected startX yPos =
  let xPos = startX + (fromIntegral index * (slotSize + slotPadding))
      
      -- El sprite del item (si existe)
      itemPic = case item of
                  Nothing -> Blank
                  Just iType -> scale 1.2 1.2 $ itemSprites ! iType
      
      -- El borde cambia si está seleccionado
      borderPic = if isSelected
                  then color selectedColor $ pictures 
                        [ rectangleWire (slotSize + 2) (slotSize + 2)
                        , rectangleWire (slotSize + 1) (slotSize + 1)
                        ]
                  else color hudDarkColor $ rectangleWire slotSize slotSize

  in translate xPos yPos $ pictures
       [ color slotBgColor $ rectangleSolid slotSize slotSize -- Fondo
       , itemPic                                              -- Item
       , borderPic                                            -- Borde
       ]