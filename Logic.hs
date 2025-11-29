module Logic where

import Types
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.State
import Control.Monad (when)
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
        playerAnimTime = 0,
        playerEquippedItem = Nothing,
        playerCooldownBallesta = 0.0,
        playerHasBoomerang = True
    },
    camera = Camera { cameraPos = spawnAtTile 25 25, cameraTarget = spawnAtTile 25 25 },
    projectiles = [],
    boomerang = Nothing,
    worldItems = [ -- Items de ejemplo
        WorldItem { itemPos = spawnAtTile 23 27, itemType = Ballesta, itemFloatTime = 0 },
        WorldItem { itemPos = spawnAtTile 24 27, itemType = Boomerang, itemFloatTime = 0 },
        WorldItem { itemPos = spawnAtTile 25 27, itemType = Espada, itemFloatTime = 0 },
        WorldItem { itemPos = spawnAtTile 26 27, itemType = Curacion, itemFloatTime = 0 },
        WorldItem { itemPos = spawnAtTile 27 27, itemType = Fuerza, itemFloatTime = 0 },
        WorldItem { itemPos = spawnAtTile 28 27, itemType = Velocidad, itemFloatTime = 0 },
        WorldItem { itemPos = spawnAtTile 29 27, itemType = Stamina, itemFloatTime = 0 }
    ],
    inputState = InputState {
        keyW = False,
        keyA = False,
        keyS = False,
        keyD = False,
        keyB = False,
        keyE = False,
        mousePos = (0, 0),
        mouseClick = False
    },
    tileMap = tiles,
    allLayers = layers,
    collisionMap = collisions,
    collisionShapes = Map.empty,
    randomSeed = 42
}


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
        EventKey (Char 'b') Down _ _ -> put gs { inputState = inp { keyB = True } }
        EventKey (Char 'b') Up _ _ -> put gs { inputState = inp { keyB = False } }
        EventKey (Char 'e') Down _ _ -> put gs { inputState = inp { keyE = True } }
        EventKey (Char 'e') Up _ _ -> put gs { inputState = inp { keyE = False } }
        EventKey (MouseButton LeftButton) Down _ pos -> put gs { inputState = inp { mouseClick = True, mousePos = pos } }
        EventKey (MouseButton LeftButton) Up _ _ -> put gs { inputState = inp { mouseClick = False } }
        EventMotion pos -> put gs { inputState = inp { mousePos = pos } }
        _ -> return ()


-- Actualizar juego
updateGame :: Float -> State GameState ()
updateGame dt = do
    updatePlayerMovement dt
    updateCamera dt
    updatePlayerCooldowns dt
    updateBoomerang dt
    updateProjectiles dt
    updateWorldItems dt
    handleItemPickup
    resetMouseClick


-- Resetea el clic del mouse
resetMouseClick :: State GameState ()
resetMouseClick = do
    gs <- get
    let inp = inputState gs
    put gs { inputState = inp { mouseClick = False } }


-- Normalizar GID (quitar flags de flip de Tiled)
normalizeGid :: Int -> Int
normalizeGid gid = gid .&. 0x1FFFFFFF

-- AABB overlap
aabbOverlap :: (Float, Float, Float, Float) -> (Float, Float, Float, Float) -> Bool
aabbOverlap (ax, ay, aw, ah) (bx, by, bw, bh) =
    not (ax + aw <= bx || bx + bw <= ax || ay + ah <= by || by + bh <= ay)

-- Verificar si un punto está dentro de un polígono (ray casting)
pointInPolygon :: (Float, Float) -> [(Float, Float)] -> Bool
pointInPolygon (px, py) pts =
    let n = length pts
        edges = zip pts (drop 1 pts ++ take 1 pts)
        crossings = length $ filter (rayIntersects (px, py)) edges
    in odd crossings
  where
    rayIntersects (rx, ry) ((x1, y1), (x2, y2))
        | y1 == y2 = False  -- Horizontal edge
        | ry < min y1 y2 = False
        | ry >= max y1 y2 = False
        | otherwise = 
            let xIntersect = x1 + (ry - y1) * (x2 - x1) / (y2 - y1)
            in rx < xIntersect

-- Verificar si un AABB colisiona con un polígono
-- Usamos: 1) alguna esquina del AABB dentro del polígono, o
--         2) algún lado del polígono cruza el AABB
aabbCollidesWithPoly :: (Float, Float, Float, Float) -> [(Float, Float)] -> Bool
aabbCollidesWithPoly (bx, by, bw, bh) poly =
    let corners = [(bx, by), (bx + bw, by), (bx, by + bh), (bx + bw, by + bh)]
        -- Alguna esquina del AABB dentro del polígono
        cornerInside = any (`pointInPolygon` poly) corners
        -- Algún vértice del polígono dentro del AABB
        polyVertexInside = any (pointInAABB (bx, by, bw, bh)) poly
        -- Algún lado del polígono cruza el AABB
        edges = zip poly (drop 1 poly ++ take 1 poly)
        edgeCrossesAABB = any (lineIntersectsAABB (bx, by, bw, bh)) edges
    in cornerInside || polyVertexInside || edgeCrossesAABB

pointInAABB :: (Float, Float, Float, Float) -> (Float, Float) -> Bool
pointInAABB (bx, by, bw, bh) (px, py) =
    px >= bx && px <= bx + bw && py >= by && py <= by + bh

-- Verificar si un segmento de línea intersecta un AABB
lineIntersectsAABB :: (Float, Float, Float, Float) -> ((Float, Float), (Float, Float)) -> Bool
lineIntersectsAABB (bx, by, bw, bh) ((x1, y1), (x2, y2)) =
    let -- Verificar intersección con los 4 lados del AABB
        left   = lineSegmentsIntersect (x1, y1) (x2, y2) (bx, by) (bx, by + bh)
        right  = lineSegmentsIntersect (x1, y1) (x2, y2) (bx + bw, by) (bx + bw, by + bh)
        bottom = lineSegmentsIntersect (x1, y1) (x2, y2) (bx, by) (bx + bw, by)
        top    = lineSegmentsIntersect (x1, y1) (x2, y2) (bx, by + bh) (bx + bw, by + bh)
    in left || right || bottom || top

-- Verificar si dos segmentos de línea se intersectan
lineSegmentsIntersect :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
lineSegmentsIntersect (x1, y1) (x2, y2) (x3, y3) (x4, y4) =
    let d = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    in if abs d < 0.0001 then False
       else let t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / d
                u = -((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / d
            in t >= 0 && t <= 1 && u >= 0 && u <= 1

-- Verificar colisión del jugador con shapes del mapa
playerCollidesAt :: GameState -> (Float, Float) -> Bool
playerCollidesAt gs (px, py) =
    let h = playerCollisionHalfSize
        -- Aplicar offset Y para que la colisión esté en los pies
        collisionY = py + playerCollisionOffsetY
        playerBox = (px - h, collisionY - h, 2 * h, 2 * h)
        layers = allLayers gs
        -- Usar primera capa para dimensiones del mapa
        firstLayer = if null layers then [] else head layers
        mapH = length firstLayer
        mapW = if null firstLayer then 0 else length (head firstLayer)
        
        -- Rango de tiles a verificar (expandido para cubrir más área)
        left = floor ((px - h - tileSize) / tileSize)
        right = ceiling ((px + h + tileSize) / tileSize)
        worldToRow wy = mapH - 1 - floor (wy / tileSize)
        topRow = worldToRow (collisionY + h + tileSize)
        bottomRow = worldToRow (collisionY - h - tileSize)
        
        colsToCheck = [max 0 left .. min (mapW - 1) right]
        rowsToCheck = [max 0 (min topRow bottomRow) .. min (mapH - 1) (max topRow bottomRow)]
        shapesMap = collisionShapes gs
        
        -- Factor de escala: tilesets son 32x32, juego usa 64x64
        shapeScale :: Float
        shapeScale = tileSize / 32.0
        
        -- Convertir shape a coordenadas mundo
        shapeToWorld :: Int -> Int -> CollisionShape -> CollisionShape
        shapeToWorld col row (CRect sx sy sw sh) =
            let sx' = sx * shapeScale
                sy' = sy * shapeScale
                sw' = sw * shapeScale
                sh' = sh * shapeScale
                tileX = fromIntegral col * tileSize
                tileY = fromIntegral (mapH - 1 - row) * tileSize
                worldX = tileX + sx'
                worldY = tileY + (tileSize - sy' - sh')
            in CRect worldX worldY sw' sh'
        shapeToWorld col row (CPoly pts) =
            let tileX = fromIntegral col * tileSize
                tileY = fromIntegral (mapH - 1 - row) * tileSize
                -- Los puntos en Tiled están en coordenadas del tile (0,0 = top-left)
                -- tileY es el BOTTOM del tile en Gloss
                -- Para convertir: worldY = tileY + (tileSize - pty_scaled)
                -- donde pty_scaled es la distancia desde el top del tile
                worldPts = map (\(ptx, pty) -> 
                    let wx = tileX + ptx * shapeScale
                        wy = tileY + tileSize - pty * shapeScale
                    in (wx, wy)) pts
            in CPoly worldPts
        
        -- Verificar colisión entre playerBox y un shape en coordenadas mundo
        checkShapeCollision :: CollisionShape -> Bool
        checkShapeCollision (CRect sx sy sw sh) = aabbOverlap playerBox (sx, sy, sw, sh)
        checkShapeCollision (CPoly pts) = aabbCollidesWithPoly playerBox pts
        
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
                                        worldShapes = map (shapeToWorld col row) validShapes
                                    in any checkShapeCollision worldShapes
        
        -- Verificar un tile en TODAS las capas
        checkTile :: Int -> Int -> Bool
        checkTile col row = any (\layer -> checkTileInLayer layer col row) layers
        
        hasValidArea :: CollisionShape -> Bool
        hasValidArea (CRect _ _ w h) = w > 0.5 && h > 0.5
        hasValidArea (CPoly pts) = length pts >= 3
    
    in or [checkTile c r | c <- colsToCheck, r <- rowsToCheck]

-- Spawn de entidad/item en coordenadas de tile
spawnAtTile :: Int -> Int -> (Float, Float)
spawnAtTile col row = (fromIntegral col * tileSize, fromIntegral row * tileSize)


-- Actualizar movimiento del jugador CON COLISIONES
updatePlayerMovement :: Float -> State GameState ()
updatePlayerMovement dt = do
    gs <- get
    let p = player gs
        inp = inputState gs
        (x, y) = playerPos p
        speed = if keyB inp then playerSprintSpeed else playerBaseSpeed
        
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
        newAnimTime = playerAnimTime p + dt

        (finalFrame, finalAnimTime) = if isMoving 
            then
                -- Animación de caminar (8 fps, 1 frame cada 0.125 s)
                let frameTime = 0.125
                    totalFrames = 4
                in if newAnimTime >= frameTime
                    then ((playerFrame p + 1) `mod` totalFrames, newAnimTime - frameTime)
                    else (playerFrame p, newAnimTime)
            else 
                -- Animación de idle (2 fps, 1 frame cada 0.5 s)
                let frameTime = 0.5
                    totalFrames = 2
                in if newAnimTime >= frameTime
                    then ((playerFrame p + 1) `mod` totalFrames, newAnimTime - frameTime)
                    else (playerFrame p, newAnimTime)
        
        newPlayer = p { 
            playerPos = finalPos, 
            playerVel = newVel,
            playerDir = newDir,
            playerFrame = finalFrame,
            playerAnimTime = finalAnimTime
        }
    
    put gs { player = newPlayer }

-- Actualizar cámara para seguir al jugador (con límites del mapa)
updateCamera :: Float -> State GameState ()
updateCamera dt = do
    gs <- get
    let pPos = playerPos (player gs)
        cam = camera gs
        currentPos = cameraPos cam
        
        -- Dimensiones del mapa
        layers = allLayers gs
        firstLayer = if null layers then [] else head layers
        mapH = length firstLayer
        mapW = if null firstLayer then 0 else length (head firstLayer)
        
        -- Tamaño del mapa en píxeles
        mapWidthPx = fromIntegral mapW * tileSize
        mapHeightPx = fromIntegral mapH * tileSize
        
        -- Mitad de la pantalla
        halfScreenW = fromIntegral screenWidth / 2
        halfScreenH = fromIntegral screenHeight / 2

        -- Interpolación lineal hacia la posición del jugador
        lerpFactor = 1.0 - (1.0 - cameraSmoothing) ** (dt * 60.0)

        (cx, cy) = currentPos
        (px, py) = pPos

        newX = cx + (px - cx) * lerpFactor
        newY = cy + (py - cy) * lerpFactor
        
        -- Limitar la cámara a los bordes del mapa
        -- La cámara no puede mostrar más allá de los límites
        clampedX = if mapWidthPx <= fromIntegral screenWidth
                   then mapWidthPx / 2  -- Centrar si el mapa es más pequeño que la pantalla
                   else max halfScreenW (min (mapWidthPx - halfScreenW) newX)
        clampedY = if mapHeightPx <= fromIntegral screenHeight
                   then mapHeightPx / 2
                   else max halfScreenH (min (mapHeightPx - halfScreenH) newY)

        newCam = cam { cameraPos = (clampedX, clampedY), cameraTarget = pPos }
        
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

        -- Verificar si el jugador tiene equipada la ballesta
        hasBallesta = case playerEquippedItem p of
            Just Ballesta -> True
            _ -> False

        -- Verificar el cooldown de la ballesta
        canShoot = playerCooldownBallesta p <= 0.0
        
        -- Crear nuevo proyectil si está disparando (usando mouseClick)
        (newProjs, newPlayer) = if mouseClick inp && hasBallesta && canShoot
                   then let dx = worldMouseX - px
                            dy = worldMouseY - py
                            len = sqrt (dx * dx + dy * dy)
                            (ndx, ndy) = if len > 0 then (dx / len, dy / len) else (1, 0)
                            projSpeed = projectileSpeed
                            newProj = Projectile {
                                projPos = (px, py),
                                projVel = (ndx * projSpeed, ndy * projSpeed),
                                projLifetime = projectileLifetime
                            }

                            updatedPlayer = p { playerCooldownBallesta = cooldownBallesta } -- Actualizar el cooldown en el jugador
                        in (newProj : projectiles gs, updatedPlayer)
                   else (projectiles gs, p)
        
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
    
    put gs { projectiles = updatedProjs, player = newPlayer }


-- Actualizar la barra de cooldown sobre el jugador
updatePlayerCooldowns :: Float -> State GameState ()
updatePlayerCooldowns dt = do
    gs <- get
    let p = player gs
        currentCD = playerCooldownBallesta p
        newCD = max 0.0 (currentCD - dt)
        newPlayer = p { playerCooldownBallesta = newCD }
    put gs { player = newPlayer }


-- Actualizar la animación de flote de los items en el mundo
updateWorldItems :: Float -> State GameState ()
updateWorldItems dt = do
    gs <- get
    let items = worldItems gs
        updatedItems = map (\item -> item { itemFloatTime = itemFloatTime item + dt }) items
    put gs { worldItems = updatedItems }


-- Manejar la recogida de items
handleItemPickup :: State GameState ()
handleItemPickup = do
    gs <- get
    let inp = inputState gs
        p = player gs
        (px, py) = playerPos p
        items = worldItems gs

    when (keyE inp) $ do
        let nearbyItems =
                filter
                    (\item ->
                        let (ix, iy) = itemPos item
                            dist = sqrt ((px - ix) ^ 2 + (py - iy) ^ 2)
                        in dist <= itemPickupRadius
                    )
                    items

        case nearbyItems of
            (item:_) -> do
                let newPlayer = case itemType item of
                        Boomerang -> p { playerEquippedItem = Just (itemType item), playerHasBoomerang = True }
                        _ -> p { playerEquippedItem = Just (itemType item) }
                    remaining = filter (/= item) items
                put gs
                    { player = newPlayer
                    , worldItems = remaining
                    , inputState = inp { keyE = False }
                    }

            [] -> return ()
    

-- BOOMERANG

-- Verifica si una posición colisiona con el entorno
positionCollidesWithWorld :: GameState -> (Float, Float) -> Bool
positionCollidesWithWorld gs (px, py) = 
    let collisionRadius = 10.0
        testPoints = [(px, py), (px + collisionRadius, py), (px - collisionRadius, py),
                      (px, py + collisionRadius), (px, py - collisionRadius)]
    in any (playerCollidesAt gs) testPoints


-- Actualiza el boomerang
updateBoomerang :: Float -> State GameState ()
updateBoomerang dt = do
    gs <- get
    let inp = inputState gs
        p = player gs
        (px, py) = playerPos p
        cam = cameraPos (camera gs)
        (mx, my) = mousePos inp

        worldMouseX = mx + fst cam
        worldMouseY = my + snd cam

        hasBoomerangEquipped = case playerEquippedItem p of
            Just Boomerang -> True
            _ -> False

        -- Boomerang equipado, el boomerang está en las manos del jugador y no existe otra instancia de boomerang en el mundo
        canThrow = hasBoomerangEquipped && playerHasBoomerang p && boomerang gs == Nothing

        -- Si se detectó el clic y se dan las condiciones, lanzar el boomerang
        newBoomerang = if mouseClick inp && canThrow
            then let dx = worldMouseX - px
                     dy = worldMouseY - py

                     len = sqrt (dx * dx + dy * dy)
                     (ndx, ndy) = if len > 0 then (dx / len, dy / len) else (1, 0)

                     newB = BoomerangProjectile {
                        boomerangPos = (px, py),
                        boomerangVel = (ndx * boomerangSpeed, ndy * boomerangSpeed),
                        boomerangState = Flying,
                        boomerangDistanceTraveled = 0.0,
                        boomerangRotation = 0.0,
                        boomerangInitialDir = (ndx, ndy)
                     }
                in Just newB
            else boomerang gs

        -- Actualizar los datos del jugador con respecto al boomerang creado
        newPlayer = if mouseClick inp && canThrow
            then p { playerHasBoomerang = False }
            else p

    -- Actualizar el estado del boomerang
    case newBoomerang of
        Nothing -> put gs { player = newPlayer }

        -- Hay un boomerang existente en la partida
        Just b -> do
            let (bx, by) = boomerangPos b
                (bvx, bvy) = boomerangVel b

                newRotation = boomerangRotation b + boomerangSpinSpeed * dt

                newBx = bx + bvx * dt
                newBy = by + bvy * dt
                newDist = boomerangDistanceTraveled b + sqrt ((bvx * dt) ^ 2 + (bvy * dt) ^ 2)

                -- Comprobar si el boomerang chocó con algo del mundo
                collided = positionCollidesWithWorld gs (newBx, newBy)

                dxToPlayer = px - newBx
                dyToPlayer = py - newBy
                distToPlayer = sqrt (dxToPlayer ^ 2 + dyToPlayer ^ 2) -- Distancia euclidiana al jugador
                (dirX, dirY) = if distToPlayer > 0.1
                    then (dxToPlayer / distToPlayer, dyToPlayer / distToPlayer)
                    else (0, 0)
                
                (newState, newVel) = case boomerangState b of
                    Flying ->
                        if collided
                            then (Returning, (dirX * boomerangSpeed, dirY * boomerangSpeed))
                        else if newDist >= boomerangMaxDistance
                            then let currentSpeed = sqrt (bvx ^ 2 + bvy ^ 2)
                                     newSpeed = max 0.0 (currentSpeed - boomerangReturnAccel * dt)
                                     (nvx, nvy) = if newSpeed > 0.1
                                        then let (dx, dy) = boomerangInitialDir b
                                            in (dx * newSpeed, dy * newSpeed)
                                        else (0, 0)
                                in if newSpeed <= 0.1
                                    then (Returning, (dirX * boomerangSpeed, dirY * boomerangSpeed))
                                    else (Flying, (nvx, nvy))
                            else (Flying, (bvx, bvy))

                    Returning -> (Returning, (dirX * boomerangSpeed, dirY * boomerangSpeed))

                caught = distToPlayer <= boomerangCatchRadius && newState == Returning

                finalBoomerang = if caught
                    then Nothing
                    else Just $ b {
                        boomerangPos = (newBx, newBy),
                        boomerangVel = newVel,
                        boomerangState = newState,
                        boomerangDistanceTraveled = newDist,
                        boomerangRotation = newRotation
                    }
                
                finalPlayer = if caught
                    then newPlayer { playerHasBoomerang = True }
                    else newPlayer
            
            put gs { boomerang = finalBoomerang, player = finalPlayer }
           