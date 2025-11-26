module Logic where

import Types
import Control.Monad.State
import Control.Monad (when)
import Graphics.Gloss.Interface.Pure.Game


-- Estado Inicial
initialGameState :: GameState
initialGameState = GameState {
    player = Player {
        playerPos = spawnAtTile 25 25, -- Centro del mapa (por ahora)
        playerVel = (0, 0),
        playerHealth = playerBaseHealth,
        playerSpeed = playerBaseSpeed,
        playerDir = DirDown,
        playerFrame = 0,
        playerAnimTime = 0
    },
    camera = Camera { cameraPos = (0, 0) },
    projectiles = [],
    inputState = InputState False False False False (0, 0) False,
    tileMap = generateTileMap 50 50 42,
    randomSeed = 42
}


-- Generar tilemap aleatorio
generateTileMap :: Int -> Int -> Int -> [[Int]]
generateTileMap width height seed = 
    [[pseudoRandom (x + y * width + seed) `mod` 32 | x <- [0..width-1]] | y <- [0..height-1]]
    where 
        pseudoRandom n = (n * 1103515245 + 12345) `mod` 2147483648 -- Generador pseudoaleatorio simple


-- Actualizar el juego (dt = delta tiempo)
updateGame :: Float -> State GameState ()
updateGame dt = do
    updatePlayerMovement dt
    updateCamera
    updateProjectiles dt
    handleShooting


-- Actualizar el movimiento del jugador
updatePlayerMovement :: Float -> State GameState ()
updatePlayerMovement dt = do
    gs <- get 
    let input = inputState gs
        p = player gs

        -- Calcular la dirección basada en el input
        dx = (if keyD input then 1 else 0) - (if keyA input then 1 else 0)
        dy = (if keyW input then 1 else 0) - (if keyS input then 1 else 0)

        -- Normalizar vector de movimiento
        len = sqrt (dx*dx + dy*dy) -- √dx² + dy²
        (ndx, ndy) = if len > 0 then (dx/len, dy/len) else (0, 0)

        -- Calcular la nueva posición
        speed = playerSpeed p
        (x, y) = playerPos p
        newPos = (x + ndx * speed * dt, y + ndy * speed * dt)

        newDir
            | dx > 0 = DirRight
            | dx < 0 = DirLeft
            | dy > 0 = DirUp
            | dy < 0 = DirDown
            | otherwise = playerDir p

        -- Animación
        animType = if dx /= 0 || dy /= 0 then Walk else Idle
        frameCount = if animType == Idle then 2 else 4

        t = playerAnimTime p + dt
        (newFrame, newt) =
            if t > 0.15 then ((playerFrame p + 1) `mod` frameCount, 0)
            else (playerFrame p, t)

        newPlayer = p {
            playerPos = newPos,
            playerVel = (ndx * speed, ndy * speed),
            playerDir = newDir,
            playerFrame = newFrame,
            playerAnimTime = newt
        }

    put gs { player = newPlayer }


-- Le permite al jugador aparecer en cualquier casilla
spawnAtTile :: Int -> Int -> Position
spawnAtTile tx ty =
    (fromIntegral tx * tileSize + tileSize/2,
     fromIntegral ty * tileSize + tileSize/2)


-- Actualizar la cámara para seguir al jugador
updateCamera :: State GameState ()
updateCamera = do
    gs <- get
    let pPos = playerPos (player gs)
        newCam = Camera { cameraPos = pPos }
    put gs { camera = newCam }


-- Actualizar proyectiles
updateProjectiles :: Float -> State GameState ()
updateProjectiles dt = do
    gs <- get
    let projs = projectiles gs
        updatedProjs = map (updateProjectile dt) projs
        aliveProjs = filter (\p -> projLifetime p > 0) updatedProjs
    put gs { projectiles = aliveProjs }

updateProjectile :: Float -> Projectile -> Projectile
updateProjectile dt proj = 
    let (px, py) = projPos proj
        (vx, vy) = projVel proj
        newPos = (px + vx * dt, py + vy * dt)
        newLifetime = projLifetime proj - dt
    in proj { projPos = newPos, projLifetime = newLifetime }
    

-- Manejar el disparo
handleShooting :: State GameState ()
handleShooting = do
    gs <- get
    let input = inputState gs
    when (mouseClick input) $ do
        let pPos = playerPos (player gs)
            mPos = mousePos input
            cam = cameraPos (camera gs)

            -- Convertir la posición del mouse a coordenadas del mundo
            worldMouseX = fst mPos + fst cam
            worldMouseY = snd mPos + snd cam

            -- Calcular la dirección del disparo
            dx = worldMouseX - fst pPos
            dy = worldMouseY - snd pPos
            len = sqrt (dx*dx + dy*dy) -- √dx² + dy²

            (ndx, ndy) = if len > 0 then (dx/len, dy/len) else (0, 1)

            newProj = Projectile {
                projPos = pPos,
                projVel = (ndx * projectileSpeed, ndy * projectileSpeed),
                projLifetime = projectileLifetime
            }

            newInput = input { mouseClick = False }

        put gs {
            projectiles = newProj : projectiles gs,
            inputState = newInput
        }


-- Funciones para manejar eventos
handleInputEvent :: Event -> State GameState ()
handleInputEvent event = do
    gs <- get
    let input = inputState gs
    case event of
        EventKey (Char 'w') Down _ _ -> put gs { inputState = input { keyW = True } }
        EventKey (Char 'w') Up _ _ -> put gs { inputState = input { keyW = False } }
        EventKey (Char 'a') Down _ _ -> put gs { inputState = input { keyA = True } }
        EventKey (Char 'a') Up _ _ -> put gs { inputState = input { keyA = False } }
        EventKey (Char 's') Down _ _ -> put gs { inputState = input { keyS = True } }
        EventKey (Char 's') Up _ _ -> put gs { inputState = input { keyS = False } }
        EventKey (Char 'd') Down _ _ -> put gs { inputState = input { keyD = True } }
        EventKey (Char 'd') Up _ _ -> put gs { inputState = input { keyD = False } }
        EventKey (MouseButton LeftButton) Down _ pos -> 
            put gs { inputState = input { mouseClick = True, mousePos = pos } }
        EventMotion pos ->
            put gs { inputState = input { mousePos = pos } }
        _ -> return ()