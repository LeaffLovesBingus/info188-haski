module Logic where

import Types
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.State
import qualified Data.Map.Strict as Map
import MapLoader (CollisionShape(..))
import Data.Bits ((.&.))

-- Estado inicial
initialGameState :: [[Int]] -> [[[Int]]] -> [[Bool]] -> GameState
initialGameState tiles layers collisions = GameState {
    player = Player { 
        playerPos = spawnAtTile 25 25, 
        playerVel = (0, 0),
        playerHealth = playerBaseHealth, 
        playerSpeed = playerBaseSpeed,
        playerDir = DirDown, 
        playerFrame = 0, 
        playerAnimTime = 0 
    },
    camera = Camera { cameraPos = spawnAtTile 25 25, cameraTarget = spawnAtTile 25 25 },
    projectiles = [],
    inputState = InputState {
        keyW = False,
        keyA = False,
        keyS = False,
        keyD = False,
        keyB = False,
        mousePos = (0, 0),
        mouseClick = False
    },
    tileMap = tiles,
    allLayers = layers,
    collisionMap = collisions,
    collisionShapes = Map.empty,
    randomSeed = 42
}

-- Normalizar GID (quitar flags de flip de Tiled)
normalizeGid :: Int -> Int
normalizeGid gid = gid .&. 0x1FFFFFFF

-- AABB overlap
aabbOverlap :: (Float, Float, Float, Float) -> (Float, Float, Float, Float) -> Bool
aabbOverlap (ax, ay, aw, ah) (bx, by, bw, bh) =
    not (ax + aw <= bx || bx + bw <= ax || ay + ah <= by || by + bh <= ay)

-- Verificar colisión del jugador con shapes del mapa
playerCollidesAt :: GameState -> (Float, Float) -> Bool
playerCollidesAt gs (px, py) =
    let h = playerCollisionHalfSize
        playerBox = (px - h, py - h, 2 * h, 2 * h)
        layers = allLayers gs
        -- Usar primera capa para dimensiones del mapa
        firstLayer = if null layers then [] else head layers
        mapH = length firstLayer
        mapW = if null firstLayer then 0 else length (head firstLayer)
        
        -- Rango de tiles a verificar (expandido para cubrir más área)
        left = floor ((px - h - tileSize) / tileSize)
        right = ceiling ((px + h + tileSize) / tileSize)
        worldToRow wy = mapH - 1 - floor (wy / tileSize)
        topRow = worldToRow (py + h + tileSize)
        bottomRow = worldToRow (py - h - tileSize)
        
        colsToCheck = [max 0 left .. min (mapW - 1) right]
        rowsToCheck = [max 0 (min topRow bottomRow) .. min (mapH - 1) (max topRow bottomRow)]
        shapesMap = collisionShapes gs
        
        -- Convertir shape a AABB en coordenadas mundo
        -- IMPORTANTE: Los tilesets usan tiles de 32x32, pero el juego usa tileSize=64
        -- Así que las coordenadas de las shapes deben escalarse por 2
        shapeScale :: Float
        shapeScale = tileSize / 32.0  -- Factor de escala (64/32 = 2)
        
        shapeToWorldAABB :: Int -> Int -> CollisionShape -> (Float, Float, Float, Float)
        shapeToWorldAABB col row (CRect sx sy sw sh) =
            let -- Escalar coordenadas del shape
                sx' = sx * shapeScale
                sy' = sy * shapeScale
                sw' = sw * shapeScale
                sh' = sh * shapeScale
                -- Esquina bottom-left del tile en coordenadas mundo
                tileX = fromIntegral col * tileSize
                tileY = fromIntegral (mapH - 1 - row) * tileSize
                -- Convertir Y de Tiled (top-down) a Gloss (bottom-up)
                worldX = tileX + sx'
                worldY = tileY + (tileSize - sy' - sh')
            in (worldX, worldY, sw', sh')
        shapeToWorldAABB col row (CPoly pts) =
            let scaledPts = map (\(px, py) -> (px * shapeScale, py * shapeScale)) pts
                xs = map fst scaledPts
                ys = map snd scaledPts
                minx = minimum xs
                maxx = maximum xs
                miny = minimum ys
                maxy = maximum ys
                tileX = fromIntegral col * tileSize
                tileY = fromIntegral (mapH - 1 - row) * tileSize
                worldY = tileY + (tileSize - maxy)
            in (tileX + minx, worldY, maxx - minx, maxy - miny)
        
        -- Verificar un tile en una capa específica
        checkTileInLayer :: [[Int]] -> Int -> Int -> Bool
        checkTileInLayer layer col row =
            if row < 0 || row >= length layer || col < 0 
            then False
            else let rowData = layer !! row
                 in if col >= length rowData
                    then False
                    else let rawGid = rowData !! col
                             gid = normalizeGid rawGid
                         in if gid <= 0
                            then False
                            else case Map.lookup gid shapesMap of
                                Nothing -> False
                                Just shapes ->
                                    let validShapes = filter hasValidArea shapes
                                        boxes = map (shapeToWorldAABB col row) validShapes
                                    in any (aabbOverlap playerBox) boxes
        
        -- Verificar un tile en TODAS las capas
        checkTile :: Int -> Int -> Bool
        checkTile col row = any (\layer -> checkTileInLayer layer col row) layers
        
        hasValidArea :: CollisionShape -> Bool
        hasValidArea (CRect _ _ w h) = w > 0.5 && h > 0.5
        hasValidArea (CPoly pts) = length pts >= 3
    
    in or [checkTile c r | c <- colsToCheck, r <- rowsToCheck]

-- Spawn del jugador en coordenadas de tile
spawnAtTile :: Int -> Int -> (Float, Float)
spawnAtTile col row = (fromIntegral col * tileSize, fromIntegral row * tileSize)

-- Manejar input
handleInputEvent :: Event -> State GameState ()
handleInputEvent event = do
    gs <- get
    let inp = inputState gs
    case event of
        EventKey (Char 'w') Down _ _ -> put gs { inputState = inp { keyW = True } }
        EventKey (Char 'w') Up _ _ -> put gs { inputState = inp { keyW = False } }
        EventKey (Char 's') Down _ _ -> put gs { inputState = inp { keyS = True } }
        EventKey (Char 's') Up _ _ -> put gs { inputState = inp { keyS = False } }
        EventKey (Char 'a') Down _ _ -> put gs { inputState = inp { keyA = True } }
        EventKey (Char 'a') Up _ _ -> put gs { inputState = inp { keyA = False } }
        EventKey (Char 'd') Down _ _ -> put gs { inputState = inp { keyD = True } }
        EventKey (Char 'd') Up _ _ -> put gs { inputState = inp { keyD = False } }
        EventKey (MouseButton LeftButton) Down _ pos -> put gs { inputState = inp { mouseClick = True, mousePos = pos } }
        EventKey (MouseButton LeftButton) Up _ _ -> put gs { inputState = inp { mouseClick = False } }
        EventMotion pos -> put gs { inputState = inp { mousePos = pos } }
        _ -> return ()

-- Actualizar juego
updateGame :: Float -> State GameState ()
updateGame dt = do
    updatePlayerMovement dt
    updateCamera dt
    updateProjectiles dt

-- Actualizar movimiento del jugador CON COLISIONES
updatePlayerMovement :: Float -> State GameState ()
updatePlayerMovement dt = do
    gs <- get
    let p = player gs
        inp = inputState gs
        (x, y) = playerPos p
        speed = playerSpeed p
        
        -- Calcular dirección de movimiento (usando los nombres correctos de campos)
        dx = (if keyD inp then 1 else 0) - (if keyA inp then 1 else 0)
        dy = (if keyW inp then 1 else 0) - (if keyS inp then 1 else 0)
        
        isMoving = dx /= 0 || dy /= 0
        
        -- Normalizar diagonal
        len = sqrt (dx * dx + dy * dy)
        (ndx, ndy) = if len > 0 then (dx / len, dy / len) else (0, 0)
        
        -- Nueva velocidad
        newVel = if isMoving then (ndx * speed, ndy * speed) else (0, 0)
        
        -- Calcular posición candidata
        candidateX = x + ndx * speed * dt
        candidateY = y + ndy * speed * dt
        
        -- Probar movimiento en X e Y por separado para permitir deslizamiento
        canMoveX = not (playerCollidesAt gs (candidateX, y))
        canMoveY = not (playerCollidesAt gs (x, candidateY))
        
        finalX = if canMoveX then candidateX else x
        finalY = if canMoveY then candidateY else y
        
        finalPos = (finalX, finalY)
        
        -- Actualizar dirección del sprite
        newDir = if not isMoving then playerDir p
                 else if abs dx > abs dy
                      then if dx > 0 then DirRight else DirLeft
                      else if dy > 0 then DirUp else DirDown
        
        -- Actualizar animación
        newAnimTime = if isMoving 
                      then playerAnimTime p + dt 
                      else 0
        newFrame = if isMoving 
                   then floor (newAnimTime * 8) `mod` 4 
                   else 0
        
        newPlayer = p { 
            playerPos = finalPos, 
            playerVel = newVel,
            playerDir = newDir,
            playerFrame = newFrame,
            playerAnimTime = newAnimTime
        }
    
    put gs { player = newPlayer }

-- Actualizar cámara para seguir al jugador
updateCamera :: Float -> State GameState ()
updateCamera dt = do
    gs <- get
    let pPos = playerPos (player gs)
        cam = camera gs
        currentPos = cameraPos cam

        -- Interpolación lineal hacia la posición del jugador
        lerpFactor = 1.0 - (1.0 - cameraSmoothing) ** (dt * 60.0)

        (cx, cy) = currentPos
        (px, py) = pPos

        newX = cx + (px - cx) * lerpFactor
        newY = cy + (py - cy) * lerpFactor

        newCam = cam { cameraPos = (newX, newY), cameraTarget = pPos }
        
    put gs { camera = newCam }

-- Actualizar proyectiles
updateProjectiles :: Float -> State GameState ()
updateProjectiles dt = do
    gs <- get
    let inp = inputState gs
        p = player gs
        (px, py) = playerPos p
        cam = cameraPos (camera gs)
        (mx, my) = mousePos inp
        
        -- Convertir posición del mouse a coordenadas mundo
        worldMouseX = mx + fst cam
        worldMouseY = my + snd cam
        
        -- Crear nuevo proyectil si está disparando (usando mouseClick)
        newProjs = if mouseClick inp
                   then let dx = worldMouseX - px
                            dy = worldMouseY - py
                            len = sqrt (dx * dx + dy * dy)
                            (ndx, ndy) = if len > 0 then (dx / len, dy / len) else (1, 0)
                            projSpeed = 500.0
                            newProj = Projectile {
                                projPos = (px, py),
                                projVel = (ndx * projSpeed, ndy * projSpeed),
                                projLifetime = 2.0
                            }
                        in newProj : projectiles gs
                   else projectiles gs
        
        -- Actualizar posiciones y filtrar proyectiles expirados
        updatedProjs = filter (\proj -> projLifetime proj > 0) $
                       map (\proj -> 
                           let (vx, vy) = projVel proj
                               (ppx, ppy) = projPos proj
                           in proj { 
                               projPos = (ppx + vx * dt, ppy + vy * dt),
                               projLifetime = projLifetime proj - dt
                           }
                       ) newProjs
    
    put gs { projectiles = updatedProjs, inputState = inp { mouseClick = False } }