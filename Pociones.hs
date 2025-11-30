module Pociones where

import Control.Monad.State
import Control.Monad (when)
import Types
import HUD.Constants (hudColor) -- Opcional, si necesitas colores

-- Configuración de balance
healAmount :: Int
healAmount = 40

speedBoostDuration :: Float
speedBoostDuration = 5.0 -- Duración en segundos del boost de velocidad

-- Función principal para manejar el uso de pociones
handlePotionUse :: State GameState ()
handlePotionUse = do
    gs <- get
    let inp = inputState gs
        p = player gs
    when (mouseClick inp) $ do
        case playerEquippedItem p of
            Just Curacion -> applyHealthPotion
            Just Velocidad -> applySpeedPotion
            _ -> return () -- No hacer nada si es otro item o nada

-- Lógica para la poción de curación
applyHealthPotion :: State GameState ()
applyHealthPotion = do
    gs <- get
    let p = player gs
    
    -- Solo consumir si no tiene la vida al máximo
    if playerHealth p < playerBaseHealth
        then do
            let newHealth = min playerBaseHealth (playerHealth p + healAmount)
                (newInv, newEquipped) = consumeItem (playerSelectedSlot p) (playerInventory p)
                
                newPlayer = p {
                    playerHealth = newHealth,
                    playerInventory = newInv,
                    playerEquippedItem = newEquipped,
                    playerItemFlashState = Showing Curacion,
                    playerItemFlashTimer = 0.5
                }
            put gs { player = newPlayer }
        else return ()

-- Lógica para la poción de velocidad
applySpeedPotion :: State GameState ()
applySpeedPotion = do
    gs <- get
    let p = player gs
        (newInv, newEquipped) = consumeItem (playerSelectedSlot p) (playerInventory p)
        
        newPlayer = p {
            playerInventory = newInv,
            playerEquippedItem = newEquipped,
            playerSpeedBoostTimer = speedBoostDuration, 
            playerItemFlashState = Showing Velocidad,
            playerItemFlashTimer = 0.5
        }
    put gs { player = newPlayer }

-- Auxiliar para eliminar el item del slot actual
consumeItem :: Int -> [Maybe ItemType] -> ([Maybe ItemType], Maybe ItemType)
consumeItem slotIndex inventory =
    let (before, after) = splitAt slotIndex inventory
        newInventory = before ++ [Nothing] ++ drop 1 after
    in (newInventory, Nothing)