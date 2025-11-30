module Logic where

import Types
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.State
import Control.Monad (when)
import qualified Data.Map.Strict as Map
import MapLoader (CollisionShape(..))
import Data.Bits ((.&.))
import Pociones

-- Estado inicial
initialGameState :: [[Int]] -> [[[Int]]] -> [[Bool]] -> GameState
initialGameState tiles layers collisions = GameState {
    currentScene = MenuScreen,
    player = Player { 
        playerPos = spawnAtTileCenter 39 32, 
        playerVel = (0, 0),
        playerHealth = playerBaseHealth, 
        playerSpeed = playerBaseSpeed,
        playerDir = DirUp, 
        playerFrame = 0, 
        playerAnimTime = 0,
        playerEquippedItem = Nothing,
        playerCooldownBallesta = 0.0,
        playerHasBoomerang = True,
        playerInventory = replicate inventorySize Nothing,
        playerSelectedSlot = 0,
        playerItemFlashTimer = 0.0,
        playerItemFlashState = NoFlash,
        playerSpeedBoostTimer = 0.0,
        playerIsTakingDamage = False,
        playerDamageAnimTimer = 0.0,
        playerDamageDirection = DamageFromFront,
        playerDamageKnockbackVel = (0, 0),
        playerIsInvulnerable = False
    },
    camera = Camera { cameraPos = spawnAtTile 39 32, cameraTarget = spawnAtTile 25 25 },
    projectiles = [],
    boomerang = Nothing,
    worldItems = [
        WorldItem { itemPos = spawnAtTileCenter 53 27, itemType = Ballesta, itemFloatTime = 0 },
        WorldItem { itemPos = spawnAtTileCenter 50 32, itemType = Boomerang, itemFloatTime = 0 },
        WorldItem { itemPos = spawnAtTileCenter 47 27, itemType = Espada, itemFloatTime = 0 }
        -- Items de ejemplo
        --,WorldItem { itemPos = spawnAtTile 26 27, itemType = Curacion, itemFloatTime = 0 },
        --WorldItem { itemPos = spawnAtTile 27 27, itemType = Fuerza, itemFloatTime = 0 },
        --WorldItem { itemPos = spawnAtTile 28 27, itemType = Velocidad, itemFloatTime = 0 },
        --WorldItem { itemPos = spawnAtTile 29 27, itemType = Stamina, itemFloatTime = 0 }
    ],
    destructibleObjects = [],
    inputState = InputState {
        keyW = False,
        keyA = False,
        keyS = False,
        keyD = False,
        keyShift = False,
        keyE = False,
        keyQ = False,
        key1 = False,
        key2 = False,
        key3 = False,
        key4 = False,
        key5 = False,
        mousePos = (0, 0),
        mouseClick = False
    },
    tileMap = tiles,
    allLayers = layers,
    collisionMap = collisions,
    collisionShapes = Map.empty,
    randomSeed = 42,
    enemies = Map.empty
}

-- NUEVO: Escanear el mapa y encontrar objetos destructibles
scanForDestructibles :: GameState -> GameState
scanForDestructibles gs = 
    let layers = allLayers gs
        shapesMap = collisionShapes gs
        
        -- Buscar en TODAS las capas
        foundObjects = concat [scanLayer layer layerIdx | (layer, layerIdx) <- zip layers [0..]]
        
        scanLayer layer layerIdx =
            let mapH = length layer
                mapW = if null layer then 0 else length (head layer)
            in concat [scanTile layer mapH col row shapesMap | 
                row <- [0..mapH-1], 
                col <- [0..mapW-1]]
        
        scanTile layer mapH col row shapesMap =
            if row >= length layer || col >= length (layer !! row)
            then []
            else
                let rawGid = (layer !! row) !! col
                    gid = normalizeGid rawGid
                in if gid `elem` destructibleGids && Map.member gid shapesMap
                   then [createDestructible col row mapH gid]
                   else []
        
        createDestructible col row mapH gid =
            let worldX = fromIntegral col * tileSize + tileSize / 2
                worldY = fromIntegral (mapH - 1 - row) * tileSize + tileSize / 2
            in DestructibleObject {
                destPos = (worldX, worldY),
                destHealth = getMaxHealth gid,
                destMaxHealth = getMaxHealth gid,
                destGid = gid,
                destTilePos = (col, row)
            }
    in gs { destructibleObjects = foundObjects }

-- Manejar input
handleInputEvent :: Event -> State GameState ()
handleInputEvent event = do
    gs <- get
    case currentScene gs of
        MenuScreen -> handleMenuInput event
        Playing    -> handlePlayingInput event
        Victory    -> handleEndScreenInput event
        Defeat     -> handleEndScreenInput event

-- Manejar input en el menú
handleMenuInput :: Event -> State GameState ()
handleMenuInput event = do
    gs <- get
    case event of
        EventKey (MouseButton LeftButton) Down _ (mx, my) -> do
            -- Verificar si se hizo clic en el botón Jugar
            if isInsideButton mx my playButtonY
                then put gs { currentScene = Playing }
            -- Verificar si se hizo clic en el botón Salir
            else if isInsideButton mx my exitButtonY
                then error "Saliendo del juego"  -- Esto cierra el juego
            else return ()
        EventKey (SpecialKey KeyEsc) Down _ _ -> error "Saliendo del juego"
        _ -> return ()

-- Manejar input en pantallas de victoria/derrota
handleEndScreenInput :: Event -> State GameState ()
handleEndScreenInput event = do
    gs <- get
    case event of
        EventKey (MouseButton LeftButton) Down _ (mx, my) -> do
            -- Verificar si se hizo clic en el botón Menu (posición Y = -280)
            if isInsideButton mx my menuButtonY
                then put gs { currentScene = MenuScreen }
                else return ()
        EventKey (SpecialKey KeyEsc) Down _ _ -> put gs { currentScene = MenuScreen }
        _ -> return ()

-- Verificar si un clic está dentro de un botón
isInsideButton :: Float -> Float -> Float -> Bool
isInsideButton mx my buttonY =
    let halfW = buttonWidth / 2
        halfH = buttonHeight / 2
    in mx >= (-halfW) && mx <= halfW && my >= (buttonY - halfH) && my <= (buttonY + halfH)

-- Constantes de botones (deben coincidir con Render.hs)
buttonWidth, buttonHeight :: Float
buttonWidth = 200
buttonHeight = 60

playButtonY, exitButtonY, menuButtonY :: Float
playButtonY = 20
exitButtonY = -80
menuButtonY = -280  -- Botón en pantallas de victoria/derrota

-- Manejar input durante el juego
handlePlayingInput :: Event -> State GameState ()
handlePlayingInput event = do
    gs <- get
    let inp = inputState gs
        p = player gs
        canMove = not (playerIsTakingDamage p)
    case event of
        EventMotion pos -> put gs { inputState = inp { mousePos = pos } }
        EventKey (SpecialKey KeyEsc) Down _ _ -> put gs { currentScene = MenuScreen }

        _ | not canMove ->
            return ()

        _ ->
            case event of
                EventKey (Char 'w') Down _ _ -> put gs { inputState = inp { keyW = True } }
                EventKey (Char 'w') Up _ _ -> put gs { inputState = inp { keyW = False } }
                EventKey (Char 's') Down _ _ -> put gs { inputState = inp { keyS = True } }
                EventKey (Char 's') Up _ _ -> put gs { inputState = inp { keyS = False } }
                EventKey (Char 'a') Down _ _ -> put gs { inputState = inp { keyA = True } }
                EventKey (Char 'a') Up _ _ -> put gs { inputState = inp { keyA = False } }
                EventKey (Char 'd') Down _ _ -> put gs { inputState = inp { keyD = True } }
                EventKey (Char 'd') Up _ _ -> put gs { inputState = inp { keyD = False } }
                EventKey (Char 'e') Down _ _ -> put gs { inputState = inp { keyE = True } }
                EventKey (Char 'e') Up _ _ -> put gs { inputState = inp { keyE = False } }
                EventKey (Char 'q') Down _ _ -> put gs { inputState = inp { keyQ = True } }
                EventKey (Char 'q') Up _ _ -> put gs { inputState = inp { keyQ = False } }
                EventKey (Char 'W') Down _ _ -> put gs { inputState = inp { keyW = True } }
                EventKey (Char 'W') Up _ _ -> put gs { inputState = inp { keyW = False } }
                EventKey (Char 'S') Down _ _ -> put gs { inputState = inp { keyS = True } }
                EventKey (Char 'S') Up _ _ -> put gs { inputState = inp { keyS = False } }
                EventKey (Char 'A') Down _ _ -> put gs { inputState = inp { keyA = True } }
                EventKey (Char 'A') Up _ _ -> put gs { inputState = inp { keyA = False } }
                EventKey (Char 'D') Down _ _ -> put gs { inputState = inp { keyD = True } }
                EventKey (Char 'D') Up _ _ -> put gs { inputState = inp { keyD = False } }
                EventKey (Char 'E') Down _ _ -> put gs { inputState = inp { keyE = True } }
                EventKey (Char 'E') Up _ _ -> put gs { inputState = inp { keyE = False } }
                EventKey (Char 'Q') Down _ _ -> put gs { inputState = inp { keyQ = True } }
                EventKey (Char 'Q') Up _ _ -> put gs { inputState = inp { keyQ = False } }
                EventKey (Char '1') Down _ _ -> put gs { inputState = inp { key1 = True } }
                EventKey (Char '1') Up _ _ -> put gs { inputState = inp { key1 = False } }
                EventKey (Char '2') Down _ _ -> put gs { inputState = inp { key2 = True } }
                EventKey (Char '2') Up _ _ -> put gs { inputState = inp { key2 = False } }
                EventKey (Char '3') Down _ _ -> put gs { inputState = inp { key3 = True } }
                EventKey (Char '3') Up _ _ -> put gs { inputState = inp { key3 = False } }
                EventKey (Char '4') Down _ _ -> put gs { inputState = inp { key4 = True } }
                EventKey (Char '4') Up _ _ -> put gs { inputState = inp { key4 = False } }
                EventKey (Char '5') Down _ _ -> put gs { inputState = inp { key5 = True } }
                EventKey (Char '5') Up _ _ -> put gs { inputState = inp { key5 = False } }
                EventKey (Char '!') Down _ _ -> put gs { inputState = inp { key1 = True } }
                EventKey (Char '!') Up _ _ -> put gs { inputState = inp { key1 = False } }
                EventKey (Char '"') Down _ _ -> put gs { inputState = inp { key2 = True } }
                EventKey (Char '"') Up _ _ -> put gs { inputState = inp { key2 = False } }
                EventKey (Char '#') Down _ _ -> put gs { inputState = inp { key3 = True } }
                EventKey (Char '#') Up _ _ -> put gs { inputState = inp { key3 = False } }
                EventKey (Char '$') Down _ _ -> put gs { inputState = inp { key4 = True } }
                EventKey (Char '$') Up _ _ -> put gs { inputState = inp { key4 = False } }
                EventKey (Char '%') Down _ _ -> put gs { inputState = inp { key5 = True } }
                EventKey (Char '%') Up _ _ -> put gs { inputState = inp { key5 = False } }
                EventKey (MouseButton LeftButton) Down _ pos -> put gs { inputState = inp { mouseClick = True, mousePos = pos } }
                EventKey (MouseButton LeftButton) Up _ _ -> put gs { inputState = inp { mouseClick = False } }
                EventKey (SpecialKey KeyShiftL) Down _ _ -> put gs { inputState = inp { keyShift = True } }
                EventKey (SpecialKey KeyShiftL) Up _ _ -> put gs { inputState = inp { keyShift = False } }

                -- DEBUG
                EventKey (SpecialKey KeyUp) Down _ _ -> do
                    let (px, py) = playerPos p
                        testEnemyPos = (px, py + 60)
                    takeDamage 10 testEnemyPos

                EventKey (SpecialKey KeyDown) Down _ _ -> do
                    let (px, py) = playerPos p
                        testEnemyPos = (px, py - 60)
                    takeDamage 10 testEnemyPos

                EventKey (SpecialKey KeyLeft) Down _ _ -> do
                    let (px, py) = playerPos p
                        testEnemyPos = (px - 60, py)
                    takeDamage 10 testEnemyPos

                EventKey (SpecialKey KeyRight) Down _ _ -> do
                    let (px, py) = playerPos p
                        testEnemyPos = (px + 60, py)
                    takeDamage 10 testEnemyPos

                _ -> return ()


-- Actualizar juego
updateGame :: Float -> State GameState ()
updateGame dt = do
    gs <- get
    case currentScene gs of
        Playing -> do
            updateDamageAnimation dt
            updateInvulnerability dt
            handleSlotSelection         -- Selección de slots con teclado
            handleItemDrop              -- Soltar items con Q
            handlePotionUse
            updateSpeedTimer dt
            updatePlayerMovement dt
            updateCamera dt
            updatePlayerCooldowns dt
            updateBoomerang dt
            updateProjectiles dt
            checkProjectileCollisions dt
            updateWorldItems dt
            handleItemPickup
            updateItemFlash dt          -- Nombre del item cuando lo recoges / seleccionas
            resetMouseClick
        _ -> return ()

updateSpeedTimer :: Float -> State GameState ()
updateSpeedTimer dt = do
    gs <- get
    let p = player gs
        newTimer = max 0.0 (playerSpeedBoostTimer p - dt)
    
    when (newTimer /= playerSpeedBoostTimer p) $ do
        let newPlayer = p { playerSpeedBoostTimer = newTimer }
        put gs { player = newPlayer }

-- Resetea el clic del mouse
resetMouseClick :: State GameState ()
resetMouseClick = do
    gs <- get
    let inp = inputState gs
    put gs { inputState = inp { mouseClick = False } }

-- Normalizar GID
normalizeGid :: Int -> Int
normalizeGid gid = gid .&. 0x1FFFFFFF

-- AABB overlap
aabbOverlap :: (Float, Float, Float, Float) -> (Float, Float, Float, Float) -> Bool
aabbOverlap (ax, ay, aw, ah) (bx, by, bw, bh) =
    not (ax + aw <= bx || bx + bw <= ax || ay + ah <= by || by + bh <= ay)

-- Verificar si un punto está dentro de un polígono
pointInPolygon :: (Float, Float) -> [(Float, Float)] -> Bool
pointInPolygon (px, py) pts =
    let edges = zip pts (drop 1 pts ++ take 1 pts)
        crossings = length $ filter (rayIntersects (px, py)) edges
    in odd crossings
  where
    rayIntersects (rx, ry) ((x1, y1), (x2, y2))
        | y1 == y2 = False
        | ry < min y1 y2 = False
        | ry >= max y1 y2 = False
        | otherwise = 
            let xIntersect = x1 + (ry - y1) * (x2 - x1) / (y2 - y1)
            in rx < xIntersect

-- Verificar colisión AABB con polígono
aabbCollidesWithPoly :: (Float, Float, Float, Float) -> [(Float, Float)] -> Bool
aabbCollidesWithPoly (bx, by, bw, bh) poly =
    let corners = [(bx, by), (bx + bw, by), (bx, by + bh), (bx + bw, by + bh)]
        cornerInside = any (`pointInPolygon` poly) corners
        polyVertexInside = any (pointInAABB (bx, by, bw, bh)) poly
        edges = zip poly (drop 1 poly ++ take 1 poly)
        edgeCrossesAABB = any (lineIntersectsAABB (bx, by, bw, bh)) edges
    in cornerInside || polyVertexInside || edgeCrossesAABB

pointInAABB :: (Float, Float, Float, Float) -> (Float, Float) -> Bool
pointInAABB (bx, by, bw, bh) (px, py) =
    px >= bx && px <= bx + bw && py >= by && py <= by + bh

lineIntersectsAABB :: (Float, Float, Float, Float) -> ((Float, Float), (Float, Float)) -> Bool
lineIntersectsAABB (bx, by, bw, bh) ((x1, y1), (x2, y2)) =
    let left   = lineSegmentsIntersect (x1, y1) (x2, y2) (bx, by) (bx, by + bh)
        right  = lineSegmentsIntersect (x1, y1) (x2, y2) (bx + bw, by) (bx + bw, by + bh)
        bottom = lineSegmentsIntersect (x1, y1) (x2, y2) (bx, by) (bx + bw, by)
        top    = lineSegmentsIntersect (x1, y1) (x2, y2) (bx, by + bh) (bx + bw, by + bh)
    in left || right || bottom || top

lineSegmentsIntersect :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
lineSegmentsIntersect (x1, y1) (x2, y2) (x3, y3) (x4, y4) =
    let d = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    in if abs d < 0.0001 then False
       else let t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / d
                u = -((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / d
            in t >= 0 && t <= 1 && u >= 0 && u <= 1

-- Verificar si un punto colisiona con collision shapes del mapa
pointCollidesWithMap :: GameState -> (Float, Float) -> Bool
pointCollidesWithMap gs (px, py) =
    let layers = allLayers gs
        firstLayer = if null layers then [] else head layers
        mapH = length firstLayer
        mapW = if null firstLayer then 0 else length (head firstLayer)
        
        col = floor (px / tileSize)
        row = mapH - 1 - floor (py / tileSize)
        
        shapesMap = collisionShapes gs
        shapeScale = tileSize / 32.0
        
        checkLayer layer =
            if row < 0 || row >= length layer || col < 0 || col >= length (layer !! row)
            then False
            else
                let rawGid = (layer !! row) !! col
                    gid = normalizeGid rawGid
                in if gid <= 0 then False
                   else case Map.lookup gid shapesMap of
                       Nothing -> False
                       Just shapes ->
                           let validShapes = filter hasValidArea shapes
                               worldShapes = map (shapeToWorld col row mapH shapeScale) validShapes
                           in any (pointInShape (px, py)) worldShapes
        
        pointInShape (ptx, pty) (CRect sx sy sw sh) =
            ptx >= sx && ptx <= sx + sw && pty >= sy && pty <= sy + sh
        pointInShape pt (CPoly pts) = pointInPolygon pt pts
        
        hasValidArea (CRect _ _ w h) = w > 0.5 && h > 0.5
        hasValidArea (CPoly pts) = length pts >= 3
        
    in any checkLayer layers

-- Convertir shape a coordenadas mundo
shapeToWorld :: Int -> Int -> Int -> Float -> CollisionShape -> CollisionShape
shapeToWorld col row mapH shapeScale (CRect sx sy sw sh) =
    let sx' = sx * shapeScale
        sy' = sy * shapeScale
        sw' = sw * shapeScale
        sh' = sh * shapeScale
        tileX = fromIntegral col * tileSize
        tileY = fromIntegral (mapH - 1 - row) * tileSize
        worldX = tileX + sx'
        worldY = tileY + (tileSize - sy' - sh')
    in CRect worldX worldY sw' sh'
shapeToWorld col row mapH shapeScale (CPoly pts) =
    let tileX = fromIntegral col * tileSize
        tileY = fromIntegral (mapH - 1 - row) * tileSize
        worldPts = map (\(ptx, pty) -> 
            let wx = tileX + ptx * shapeScale
                wy = tileY + tileSize - pty * shapeScale
            in (wx, wy)) pts
    in CPoly worldPts

-- Verificar colisión del jugador
playerCollidesAt :: GameState -> (Float, Float) -> Bool
playerCollidesAt gs (px, py) =
    let h = playerCollisionHalfSize
        collisionY = py + playerCollisionOffsetY
        playerBox = (px - h, collisionY - h, 2 * h, 2 * h)
        layers = allLayers gs
        firstLayer = if null layers then [] else head layers
        mapH = length firstLayer
        mapW = if null firstLayer then 0 else length (head firstLayer)
        
        left = floor ((px - h - tileSize) / tileSize)
        right = ceiling ((px + h + tileSize) / tileSize)
        worldToRow wy = mapH - 1 - floor (wy / tileSize)
        topRow = worldToRow (collisionY + h + tileSize)
        bottomRow = worldToRow (collisionY - h - tileSize)
        
        colsToCheck = [max 0 left .. min (mapW - 1) right]
        rowsToCheck = [max 0 (min topRow bottomRow) .. min (mapH - 1) (max topRow bottomRow)]
        shapesMap = collisionShapes gs
        
        shapeScale = tileSize / 32.0
        
        checkShapeCollision :: CollisionShape -> Bool
        checkShapeCollision (CRect sx sy sw sh) = aabbOverlap playerBox (sx, sy, sw, sh)
        checkShapeCollision (CPoly pts) = aabbCollidesWithPoly playerBox pts
        
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
                                        worldShapes = map (shapeToWorld col row mapH shapeScale) validShapes
                                    in any checkShapeCollision worldShapes
        
        checkTile :: Int -> Int -> Bool
        checkTile col row = any (\layer -> checkTileInLayer layer col row) layers
        
        hasValidArea :: CollisionShape -> Bool
        hasValidArea (CRect _ _ w h) = w > 0.5 && h > 0.5
        hasValidArea (CPoly pts) = length pts >= 3
    
    in or [checkTile c r | c <- colsToCheck, r <- rowsToCheck]

-- Spawn de entidad en coordenadas de tile (DEVUELVE LA ESQUINA SUPERIOR IZQUIERDA DEL TILE)
spawnAtTile :: Int -> Int -> (Float, Float)
spawnAtTile col row = (fromIntegral col * tileSize, fromIntegral row * tileSize)

-- Spawn de entidad en coordenadas de tile (DEVUELVE EL CENTRO DEL TILE)
spawnAtTileCenter :: Int -> Int -> (Float, Float)
spawnAtTileCenter col row = ((fromIntegral col * tileSize) + tileSize/2, (fromIntegral row * tileSize) + tileSize/2)

-- Actualizar movimiento del jugador
updatePlayerMovement :: Float -> State GameState ()
updatePlayerMovement dt = do
    gs <- get
    
    let p = player gs

    if playerIsTakingDamage p
        then return ()  -- No hacer nada
        else do
            let inp = inputState gs
                (x, y) = playerPos p
                base = if keyShift inp then playerSprintSpeed else playerBaseSpeed
                speed = if playerSpeedBoostTimer p > 0 then base + 200 else base
        
                dx = (if keyD inp then 1 else 0) - (if keyA inp then 1 else 0)
                dy = (if keyW inp then 1 else 0) - (if keyS inp then 1 else 0)
                
                isMoving = dx /= 0 || dy /= 0
                
                len = sqrt (dx * dx + dy * dy)
                (ndx, ndy) = if len > 0 then (dx / len, dy / len) else (0, 0)
                
                newVel = if isMoving then (ndx * speed, ndy * speed) else (0, 0)
                
                candidateX = x + ndx * speed * dt
                candidateY = y + ndy * speed * dt
                
                canMoveX = not (playerCollidesAt gs (candidateX, y))
                canMoveY = not (playerCollidesAt gs (x, candidateY))
                
                finalX = if canMoveX then candidateX else x
                finalY = if canMoveY then candidateY else y
                
                finalPos = (finalX, finalY)
                
                newDir = if not isMoving then playerDir p
                        else if abs dx > abs dy
                            then if dx > 0 then DirRight else DirLeft
                            else if dy > 0 then DirUp else DirDown
                
                newAnimTime = playerAnimTime p + dt

                (finalFrame, finalAnimTime) = if isMoving 
                    then
                        let frameTime = 0.125
                            totalFrames = 4
                        in if newAnimTime >= frameTime
                            then ((playerFrame p + 1) `mod` totalFrames, newAnimTime - frameTime)
                            else (playerFrame p, newAnimTime)
                    else 
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

-- Actualizar cámara
updateCamera :: Float -> State GameState ()
updateCamera dt = do
    gs <- get
    let pPos = playerPos (player gs)
        cam = camera gs
        currentPos = cameraPos cam
        
        layers = allLayers gs
        firstLayer = if null layers then [] else head layers
        mapH = length firstLayer
        mapW = if null firstLayer then 0 else length (head firstLayer)
        
        mapWidthPx = fromIntegral mapW * tileSize
        mapHeightPx = fromIntegral mapH * tileSize
        
        halfScreenW = fromIntegral screenWidth / 2
        halfScreenH = fromIntegral screenHeight / 2

        lerpFactor = 1.0 - (1.0 - cameraSmoothing) ** (dt * 60.0)

        (cx, cy) = currentPos
        (px, py) = pPos

        newX = cx + (px - cx) * lerpFactor
        newY = cy + (py - cy) * lerpFactor
        
        clampedX = if mapWidthPx <= fromIntegral screenWidth
                   then mapWidthPx / 2
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
        
        worldMouseX = mx + fst cam
        worldMouseY = my + snd cam

        hasBallesta = case playerEquippedItem p of
            Just Ballesta -> True
            _ -> False

        canShoot = playerCooldownBallesta p <= 0.0
        
        (newProjs, newPlayer) = if mouseClick inp && hasBallesta && canShoot && not (playerIsTakingDamage p)
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
                            updatedPlayer = p { playerCooldownBallesta = cooldownBallesta }
                        in (newProj : projectiles gs, updatedPlayer)
                   else (projectiles gs, p)
        
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

-- NUEVO: Verificar colisiones de proyectiles
checkProjectileCollisions :: Float -> State GameState ()
checkProjectileCollisions dt = do
    gs <- get
    let projs = projectiles gs
        objs = destructibleObjects gs
        
        -- Filtrar proyectiles que colisionan
        -- checkProj usa `gs`, así que se define aquí dentro
        checkProj (projsAcc, objsAcc, itemsAcc) proj =
            let (px, py) = projPos proj
                hitMap = pointCollidesWithMap gs (px, py)
                maybeHitObj = findHitObject (px, py) objsAcc
            in case maybeHitObj of
                Just obj ->
                    let newHealth = destHealth obj - arrowDamage
                        updatedObj = obj { destHealth = newHealth }
                        updatedObjs = map (\o -> if o == obj then updatedObj else o) objsAcc
                    in (projsAcc, updatedObjs, itemsAcc)
                Nothing -> if hitMap then (projsAcc, objsAcc, itemsAcc) else (proj : projsAcc, objsAcc, itemsAcc)

        (survivingProjs, damagedObjs, newItems) = foldl checkProj ([], objs, []) projs
        
        -- Filtrar objetos destruidos y actualizar mapa
        (aliveObjs, destroyedObjs) = span (\obj -> destHealth obj > 0) damagedObjs
        finalObjs = filter (\obj -> destHealth obj > 0) damagedObjs
        
        -- Generar items de loot
        lootItems = [createLootItem obj | obj <- damagedObjs, destHealth obj <= 0]
        
        -- Eliminar tiles destruidos
        updatedLayers = foldl removeTileFromLayers (allLayers gs) 
                        [obj | obj <- damagedObjs, destHealth obj <= 0]
    
    put gs {
        projectiles = survivingProjs,
        destructibleObjects = finalObjs,
        worldItems = worldItems gs ++ lootItems,
        allLayers = updatedLayers
    }

-- Encontrar objeto destructible en una posición
findHitObject :: (Float, Float) -> [DestructibleObject] -> Maybe DestructibleObject
findHitObject (px, py) objs =
    let hits = filter (\obj -> 
            let (ox, oy) = destPos obj
                dist = sqrt ((px - ox) ** 2 + (py - oy) ** 2)
            in dist < tileSize * 0.8  -- Radio de colisión
            ) objs
    in if null hits then Nothing else Just (head hits)

-- Crear item de loot
createLootItem :: DestructibleObject -> WorldItem
createLootItem obj = WorldItem {
    itemPos = destPos obj,
    itemType = getLootItem (destGid obj),
    itemFloatTime = 0
}

-- Eliminar SOLO los tiles en las posiciones específicas del objeto
removeTileFromLayers :: [[[Int]]] -> DestructibleObject -> [[[Int]]]
removeTileFromLayers layers obj =
    let (baseCol, baseRow) = destTilePos obj
        offsets = getDestructibleOffsets (destGid obj)
        positionsToRemove = map (\(offsetX, offsetY) -> (baseCol + offsetX, baseRow + offsetY)) offsets
        background = if null layers then [] else head layers

        replaceInLayer :: Int -> [[Int]] -> [[Int]]
        replaceInLayer idx layer
            | idx == 0 = layer  -- keep background layer as-is
            | otherwise =
                [ [ if (c, r) `elem` positionsToRemove
                      then 0  -- make tile empty so underlying layers (background) show through
                      else gid
                  | (c, gid) <- zip [0..] rowData ]
                | (r, rowData) <- zip [0..] layer ]

    in zipWith replaceInLayer [0..] layers

-- Actualizar cooldowns
updatePlayerCooldowns :: Float -> State GameState ()
updatePlayerCooldowns dt = do
    gs <- get
    let p = player gs
        currentCD = playerCooldownBallesta p
        newCD = max 0.0 (currentCD - dt)
        newPlayer = p { playerCooldownBallesta = newCD }
    put gs { player = newPlayer }

-- Actualizar animación de items
updateWorldItems :: Float -> State GameState ()
updateWorldItems dt = do
    gs <- get
    let items = worldItems gs
        updatedItems = map (\item -> item { itemFloatTime = itemFloatTime item + dt }) items
    put gs { worldItems = updatedItems }


-- Buscar primer slot vacío del inventario
findEmptySlot :: [Maybe ItemType] -> Int -> Maybe Int
findEmptySlot [] _ = Nothing
findEmptySlot (slot:rest) idx = case slot of
    Nothing -> Just idx
    Just _ -> findEmptySlot rest (idx + 1)


-- INVENTARIO


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
                let inventory = playerInventory p
                    itemToAdd = itemType item
                    
                    -- Buscar primer slot vacío
                    findEmptySlot :: [Maybe ItemType] -> Int -> Maybe Int
                    findEmptySlot [] _ = Nothing
                    findEmptySlot (slot:rest) idx = case slot of
                        Nothing -> Just idx
                        Just _ -> findEmptySlot rest (idx + 1)
                    
                    emptySlotIdx = findEmptySlot inventory 0
                
                case emptySlotIdx of
                    -- Hay un slot vacío, añadir el item ahí
                    Just idx -> do
                        let newInventory = updateInventoryAt idx (Just itemToAdd) inventory
                            -- Actualizar el boomerang flag si es necesario
                            hasBoomerang' = itemToAdd == Boomerang || playerHasBoomerang p
                            newPlayer = p { 
                                playerInventory = newInventory,
                                playerSelectedSlot = idx,  -- Cambiar al slot donde se añadió
                                playerEquippedItem = Just itemToAdd,  -- Equipar el nuevo item
                                playerHasBoomerang = hasBoomerang',
                                playerItemFlashState = Showing itemToAdd,
                                playerItemFlashTimer = itemNameFlashDuration
                            }
                            remaining = filter (/= item) items
                        
                        put gs { player = newPlayer, worldItems = remaining, inputState = inp { keyE = False } }
                    
                    -- No hay slots vacíos, reemplazar el slot actual
                    Nothing -> do
                        let currentSlot = playerSelectedSlot p
                            oldItem = inventory !! currentSlot
                            newInventory = updateInventoryAt currentSlot (Just itemToAdd) inventory
                            hasBoomerang' = itemToAdd == Boomerang || 
                                           (playerHasBoomerang p && oldItem /= Just Boomerang)
                            newPlayer = p { 
                                playerInventory = newInventory,
                                playerEquippedItem = Just itemToAdd,
                                playerHasBoomerang = hasBoomerang',
                                playerItemFlashState = Showing itemToAdd,
                                playerItemFlashTimer = itemNameFlashDuration
                            }
                            
                            -- Crear el item que se dropea
                            droppedItem = case oldItem of
                                Just iType -> WorldItem { 
                                    itemPos = (px, py), 
                                    itemType = iType, 
                                    itemFloatTime = 0 
                                }
                                Nothing -> item  -- No debería pasar, pero por seguridad
                            
                            remaining = filter (/= item) items
                            newItems = droppedItem : remaining
                        
                        put gs { player = newPlayer, worldItems = newItems, inputState = inp { keyE = False } }

            [] -> return ()
    where
        updateInventoryAt :: Int -> Maybe ItemType -> [Maybe ItemType] -> [Maybe ItemType]
        updateInventoryAt _ _ [] = []
        updateInventoryAt 0 newItem (_:rest) = newItem : rest
        updateInventoryAt n newItem (x:rest) = x : updateInventoryAt (n-1) newItem rest
    
    

-- Manejar selección de slots con teclado y scrollwheel
handleSlotSelection :: State GameState ()
handleSlotSelection = do
    gs <- get
    let inp = inputState gs
        p = player gs
        currentSlot = playerSelectedSlot p
        inventory = playerInventory p
        
        -- Determinar nuevo slot basado en input
        newSlotMaybe = if key1 inp then Just 0
                       else if key2 inp then Just 1
                       else if key3 inp then Just 2
                       else if key4 inp then Just 3
                       else if key5 inp then Just 4
                       else Nothing
    
    case newSlotMaybe of
        Nothing -> return ()
        Just newSlot -> when (newSlot /= currentSlot) $ do
            let newEquippedItem = inventory !! newSlot
                -- Resetear flash si cambia de slot
                (newFlashState, newFlashTimer) = case newEquippedItem of
                    Just iType -> (Showing iType, itemNameFlashDuration)
                    Nothing -> (NoFlash, 0)
                
                newPlayer = p {
                    playerSelectedSlot = newSlot,
                    playerEquippedItem = newEquippedItem,
                    playerItemFlashState = newFlashState,
                    playerItemFlashTimer = newFlashTimer
                }
                
                -- Resetear las teclas de input
                newInput = inp { 
                    key1 = False, key2 = False, key3 = False, 
                    key4 = False, key5 = False
                }
            
            put gs { player = newPlayer, inputState = newInput }


-- Soltar item al presionar Q
handleItemDrop :: State GameState ()
handleItemDrop = do
    gs <- get
    let inp = inputState gs
        p = player gs
        
    when (keyQ inp) $ do
        let currentSlot = playerSelectedSlot p
            inventory = playerInventory p
            currentItem = inventory !! currentSlot
            
        case currentItem of
            Nothing -> put gs { inputState = inp { keyQ = False } }
            Just iType -> do
                let (px, py) = playerPos p
                    droppedItem = WorldItem {
                        itemPos = (px, py),
                        itemType = iType,
                        itemFloatTime = 0
                    }
                    
                    -- Actualizar inventario
                    newInventory = updateInventoryAt currentSlot Nothing inventory
                    
                    -- Actualizar boomerang flag si se dropea el boomerang
                    hasBoomerang' = if iType == Boomerang then False else playerHasBoomerang p
                    
                    newPlayer = p {
                        playerInventory = newInventory,
                        playerEquippedItem = Nothing,
                        playerHasBoomerang = hasBoomerang',
                        playerItemFlashState = NoFlash,
                        playerItemFlashTimer = 0
                    }
                    
                    newItems = droppedItem : worldItems gs
                
                put gs { 
                    player = newPlayer, 
                    worldItems = newItems,
                    inputState = inp { keyQ = False }
                }
  where
    updateInventoryAt :: Int -> Maybe ItemType -> [Maybe ItemType] -> [Maybe ItemType]
    updateInventoryAt _ _ [] = []
    updateInventoryAt 0 newItem (_:rest) = newItem : rest
    updateInventoryAt n newItem (x:rest) = x : updateInventoryAt (n-1) newItem rest


-- Actualizar el flash del nombre del item
updateItemFlash :: Float -> State GameState ()
updateItemFlash dt = do
    gs <- get
    let p = player gs
        flashState = playerItemFlashState p
        flashTimer = playerItemFlashTimer p
    
    case flashState of
        NoFlash -> return ()
        
        Showing iType -> do
            let newTimer = flashTimer - dt
            if newTimer <= 0
                then do
                    -- Pasar a FadingOut
                    let newPlayer = p {
                        playerItemFlashState = FadingOut iType itemNameFadeOutDuration,
                        playerItemFlashTimer = itemNameFadeOutDuration
                    }
                    put gs { player = newPlayer }
                else do
                    let newPlayer = p { playerItemFlashTimer = newTimer }
                    put gs { player = newPlayer }
        
        FadingOut iType fadeTime -> do
            let newTimer = flashTimer - dt
            if newTimer <= 0
                then do
                    -- Terminar el flash
                    let newPlayer = p {
                        playerItemFlashState = NoFlash,
                        playerItemFlashTimer = 0
                    }
                    put gs { player = newPlayer }
                else do
                    let newPlayer = p { playerItemFlashTimer = newTimer }
                    put gs { player = newPlayer }


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
        canThrow = hasBoomerangEquipped && playerHasBoomerang p && boomerang gs == Nothing && not (playerIsTakingDamage p)

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

                -- Comprobar si el boomerang chocó con algo del mundo O con un objeto destructible
                maybeHitObj = findHitObject (newBx, newBy) (destructibleObjects gs)
                collided = positionCollidesWithWorld gs (newBx, newBy) || maybeHitObj /= Nothing

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

                -- Si golpeó un objeto destructible, aplicamos daño
                gsAfterHit = case maybeHitObj of
                    Nothing -> gs
                    Just obj ->
                        let newHealth = destHealth obj - boomerangDamage
                            updatedObj = obj { destHealth = newHealth }
                            updatedObjs = map (\o -> if o == obj then updatedObj else o) (destructibleObjects gs)
                            -- Generar loot si murió
                            lootItems = if newHealth <= 0 then [createLootItem obj] else []
                            -- Actualizar capas si se destruyó
                            updatedLayers = if newHealth <= 0 then foldl removeTileFromLayers (allLayers gs) [obj] else allLayers gs
                            finalObjs = filter (\o -> destHealth o > 0) updatedObjs
                        in gs { destructibleObjects = finalObjs, worldItems = worldItems gs ++ lootItems, allLayers = updatedLayers }

            put gsAfterHit { boomerang = finalBoomerang, player = finalPlayer }
           

-- DAÑO

-- Función para calcular la dirección del daño basado en posiciones
calculateDamageDirection :: Position -> Position -> DamageDirection
calculateDamageDirection playerPos enemyPos =
    let (px, py) = playerPos
        (ex, ey) = enemyPos
        dx = ex - px
        dy = ey - py
    in if abs dx > abs dy
       then if dx > 0 then DamageFromRight else DamageFromLeft
       else if dy > 0 then DamageFromBack else DamageFromFront

-- Convertir DamageDirection a Direction para la animación
damageDirectionToPlayerDir :: DamageDirection -> Direction
damageDirectionToPlayerDir DamageFromFront = DirDown
damageDirectionToPlayerDir DamageFromBack = DirUp
damageDirectionToPlayerDir DamageFromLeft = DirLeft
damageDirectionToPlayerDir DamageFromRight = DirRight

-- Calcular velocidad de knockback (ease-out)
calculateKnockbackVelocity :: DamageDirection -> Velocity
calculateKnockbackVelocity dir =
    let speed = damageKnockbackDistance / damageAnimationDuration
    in case dir of
        DamageFromFront -> (0, speed)   -- Empujar hacia abajo
        DamageFromBack -> (0, -speed)     -- Empujar hacia arriba
        DamageFromLeft -> (speed, 0)     -- Empujar hacia derecha
        DamageFromRight -> (-speed, 0)   -- Empujar hacia izquierda

-- Función principal para infligir daño al jugador
takeDamage :: Int -> Position -> State GameState ()
takeDamage damage enemyPos = do
    gs <- get
    let p = player gs
        
        -- Solo aplicar daño si no está invulnerable y no está tomando daño
        canTakeDamage = not (playerIsInvulnerable p) && not (playerIsTakingDamage p)
    
    when canTakeDamage $ do
        let damageDir = calculateDamageDirection (playerPos p) enemyPos
            animDir = damageDirectionToPlayerDir damageDir
            knockbackVel = calculateKnockbackVelocity damageDir
            
            newHealth = max 0 (playerHealth p - damage)
            
            updatedPlayer = p {
                playerHealth = newHealth,
                playerIsTakingDamage = True,
                playerDamageAnimTimer = damageAnimationDuration,
                playerDamageDirection = damageDir,
                playerDamageKnockbackVel = knockbackVel,
                playerIsInvulnerable = True,
                playerDir = animDir,
                playerFrame = 0,
                playerAnimTime = 0
            }
        
        put gs { player = updatedPlayer }

-- Actualizar la animación de daño
updateDamageAnimation :: Float -> State GameState ()
updateDamageAnimation dt = do
    gs <- get
    let p = player gs
    
    when (playerIsTakingDamage p) $ do
        let timer = playerDamageAnimTimer p
            newTimer = max 0 (timer - dt)
            
            progress = 1.0 - (newTimer / damageAnimationDuration)
            easeOutProgress = 1.0 - (1.0 - progress) ** 2
            
            -- Aplicar knockback con ease-out
            (kvx, kvy) = playerDamageKnockbackVel p
            currentKnockback = (kvx * (1.0 - easeOutProgress), kvy * (1.0 - easeOutProgress))
            (px, py) = playerPos p
            newPos = (px + fst currentKnockback * dt, py + snd currentKnockback * dt)
            
            -- Actualizar frame de animación
            frameTime = damageAnimationDuration / 4.0  -- 4 frames
            newAnimTime = playerAnimTime p + dt
            (newFrame, finalAnimTime) = if newAnimTime >= frameTime
                then ((playerFrame p + 1) `mod` 4, newAnimTime - frameTime)
                else (playerFrame p, newAnimTime)
            
            -- Verificar si la animación terminó
            animationEnded = newTimer <= 0
            
            updatedPlayer = if animationEnded
                then p {
                    playerPos = newPos,
                    playerIsTakingDamage = False,
                    playerDamageAnimTimer = 0,
                    playerFrame = 0,
                    playerAnimTime = 0
                    -- playerIsInvulnerable se mantiene en True
                }
                else p {
                    playerPos = newPos,
                    playerDamageAnimTimer = newTimer,
                    playerFrame = newFrame,
                    playerAnimTime = finalAnimTime
                }
        
        put gs { player = updatedPlayer }

-- Actualizar invulnerabilidad
updateInvulnerability :: Float -> State GameState ()
updateInvulnerability dt = do
    gs <- get
    let p = player gs
        inp = inputState gs
    
    -- Si está invulnerable pero NO está en animación de daño
    when (playerIsInvulnerable p && not (playerIsTakingDamage p)) $ do
        let updatedPlayer = p { playerIsInvulnerable = False }
            updatedInput = resetInputState inp

        put gs { player = updatedPlayer, inputState = updatedInput } 


-- Resetear inputs
resetInputState :: InputState -> InputState
resetInputState inp = inp 
    { keyW = False
    , keyA = False
    , keyS = False
    , keyD = False
    , keyShift = False
    , keyE = False
    , keyQ = False
    , key1 = False
    , key2 = False
    , key3 = False
    , key4 = False
    , key5 = False
    , mouseClick = False
    }