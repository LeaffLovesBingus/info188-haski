module Enemys where

import Control.Monad.State
import qualified Data.Map as M
import Types
import System.IO.Unsafe (unsafePerformIO)

-------------------------------------------
-- INTEGRACIÓN CON GAMESTATE
-------------------------------------------

-- Actualizar enemigos en el contexto del GameState
modifyEnemies :: (Enemies -> Enemies) -> State GameState ()
modifyEnemies f = modify $ \gs -> gs { enemies = f (enemies gs) }

-- Actualizar tracking de TODOS los enemigos hacia el jugador
updateEnemyTracking :: Float -> State GameState ()
updateEnemyTracking dt = do
    gs <- get
    let pPos = playerPos (player gs)
        currentEnemies = enemies gs
        -- Aplicar tracking a todos los enemigos
        updatedEnemies = M.map (trackTowardsPlayer pPos) currentEnemies
    put gs { enemies = updatedEnemies }
  where
    trackTowardsPlayer :: Position -> EnemyState -> EnemyState
    trackTowardsPlayer (px, py) enemy =
        let (ex, ey) = position enemy
            dir = normalize (px - ex, py - ey)
            spd = speed enemy
            newVel = (fst dir * spd, snd dir * spd)
        in enemy { velocity = newVel }

-- Actualizar movimiento de TODOS los enemigos
updateEnemyMovementAll :: Float -> State GameState ()
updateEnemyMovementAll dt = do
    gs <- get
    let currentEnemies = enemies gs
        updatedEnemies = M.map (moveEnemy dt) currentEnemies
    put gs { enemies = updatedEnemies }
  where
    moveEnemy :: Float -> EnemyState -> EnemyState
    moveEnemy dt enemy =
        let (x, y) = position enemy
            (vx, vy) = velocity enemy
        in enemy { position = (x + vx * dt, y + vy * dt) }

-- Resolver colisiones entre enemigos (VERSIÓN LIMPIA CON DEBUG)
resolveAllEnemyCollisions :: State GameState ()
resolveAllEnemyCollisions = do

    -- Debug: verificar que se ejecuta
    let _ = unsafePerformIO $ putStrLn ">>> resolveAllEnemyCollisions CALLED <<<"
    
    gs <- get
    let currentEnemies = enemies gs
        enemyList = M.elems currentEnemies
        
        -- Debug: mostrar posiciones
        _ = unsafePerformIO $ do
            putStrLn "=== CHECKING COLLISIONS ==="
            mapM_ (\e -> putStrLn $ "Enemy " ++ show (enemy_id e) ++ 
                                    " at " ++ show (position e) ++ 
                                    " radius " ++ show (radius e)) enemyList
            return ()
        
        collidingPairs = [(e1, e2) | e1 <- enemyList, 
                                     e2 <- enemyList,
                                     enemy_id e1 < enemy_id e2,
                                     enemiesCollide e1 e2]
        
        -- Debug: mostrar colisiones detectadas
        _ = unsafePerformIO $ do
            if null collidingPairs
                then putStrLn "No collisions detected"
                else do
                    putStrLn $ "Found " ++ show (length collidingPairs) ++ " collisions:"
                    mapM_ (\(e1, e2) -> putStrLn $ "  Enemy " ++ show (enemy_id e1) ++ 
                                                   " <-> Enemy " ++ show (enemy_id e2)) collidingPairs
            return ()
        
        finalEnemies = iterateCollisionResolution 3 currentEnemies
    put gs { enemies = finalEnemies }
  where
    -- Iterar para resolver colisiones
    iterateCollisionResolution :: Int -> Enemies -> Enemies
    iterateCollisionResolution 0 enems = enems
    iterateCollisionResolution n enems =
        let enemyList = M.elems enems
            collidingPairs = [(e1, e2) | e1 <- enemyList, 
                                         e2 <- enemyList,
                                         enemy_id e1 < enemy_id e2,
                                         enemiesCollide e1 e2]
            resolvedEnemies = foldl separatePair enems collidingPairs
        in if null collidingPairs
           then resolvedEnemies
           else iterateCollisionResolution (n - 1) resolvedEnemies
    
    -- Separar un par de enemigos (SOLO UNA DEFINICIÓN)
    separatePair :: Enemies -> (EnemyState, EnemyState) -> Enemies
    separatePair m (e1, e2) =
        let (e1', e2') = separateEnemies e1 e2
            -- Debug: mostrar separación
            _ = unsafePerformIO $ do
                putStrLn $ "Separating " ++ show (enemy_id e1) ++ " and " ++ show (enemy_id e2)
                putStrLn $ "  Before: " ++ show (position e1) ++ " -> " ++ show (position e2)
                putStrLn $ "  After:  " ++ show (position e1') ++ " -> " ++ show (position e2')
                return ()
        in M.insert (enemy_id e2') e2' $ M.insert (enemy_id e1') e1' m

-- Empujar enemigos lejos del jugador
pushEnemiesFromPlayer :: State GameState ()
pushEnemiesFromPlayer = do
    gs <- get
    let pPos = playerPos (player gs)
        pRadius = playerRadius
        currentEnemies = enemies gs
        updatedEnemies = M.map (pushAway pPos pRadius) currentEnemies
    put gs { enemies = updatedEnemies }
  where
    pushAway :: Position -> Float -> EnemyState -> EnemyState
    pushAway playerPos playerRadius enemy =
        if enemyCollidesWithPlayer playerPos playerRadius enemy
        then
            let (ex, ey) = position enemy
                (px, py) = playerPos
                (dx, dy) = (ex - px, ey - py)
                dist = sqrt (dx*dx + dy*dy)
                
                (nx, ny) = if dist < 0.0001
                            then (1, 0)
                            else (dx / dist, dy / dist)
                    
                minDist = radius enemy + playerRadius
                overlap = minDist - dist
                pushDistance = overlap + 0.5
                newPos = (ex + nx * pushDistance, ey + ny * pushDistance)
                
            in enemy { position = newPos }
        else enemy

-------------------------------------------
-- OPERACIONES PARA EL STATE DE UN ENEMY
-------------------------------------------
-- daño que recibe el enemigo
damageReceived :: EnemyID -> Int -> State Enemies ()
damageReceived eID dmg = modify $ \m ->
    case M.lookup eID m of
        Nothing -> m
        Just enemy ->
            let newHealth = health enemy - dmg
            in if newHealth <= 0
                then M.delete eID m
                else M.insert eID (enemy { health = newHealth }) m

-------------------------------------------
-- TRACKEO
-------------------------------------------

-- trackPlayer de un enemigo individual
trackPlayer :: Position -> EnemyID -> State Enemies ()
trackPlayer playerPos eID = modify $ \m ->
    case M.lookup eID m of
        Nothing     -> m
        Just enemy  ->
            let (ex, ey) = position enemy
                (px, py) = playerPos
                dir      = normalize (px - ex, py - ey)
                speed    = enemyBaseSpeed
                newVel   = (fst dir * speed, snd dir * speed)
                enemy'   = enemy { velocity = newVel }
            in M.insert eID enemy' m

-- trackPlayer para TODOS los enemigos
trackAllEnemies :: Position -> State Enemies ()
trackAllEnemies playerPos = modify $ M.map (updateEnemyTracking playerPos)
    where
        updateEnemyTracking :: Position -> EnemyState -> EnemyState
        updateEnemyTracking (px, py) enemy =
            let (ex, ey) = position enemy
                dir = normalize (px - ex, py - ey)
                spd = speed enemy
                newVel = (fst dir * spd, snd dir * spd)
            in enemy { velocity = newVel }

-------------------------------------------
-- MOVIMIENTO
-------------------------------------------
-- actualiza el movimiento de un enemy
updateEnemyMovement :: Float -> EnemyID -> State Enemies ()
updateEnemyMovement dt eID = modify $ \m ->
    case M.lookup eID m of
        Nothing -> m
        Just enemy ->
            let (x, y) = position enemy
                (vx, vy) = velocity enemy
                newPos = (x + vx * dt, y + vy * dt)
                enemy' = enemy { position = newPos }
            in M.insert eID enemy' m

-- actualiza el movimiento de todos los enemy
updateAllEnemyMovement :: Float -> State Enemies ()
updateAllEnemyMovement dt = modify $ M.map updatePos
    where
        updatePos :: EnemyState -> EnemyState
        updatePos enemy =
            let (x, y) = position enemy
                (vx, vy) = velocity enemy
            in enemy { position = (x + vx * dt, y + vy * dt) }

-------------------------------------------
-- CONSTANTES
-------------------------------------------
-- speed BASE para enemigos
enemyBaseSpeed :: Float
enemyBaseSpeed = 150

-- radio por defecto según tipo de enemigo
defaultRadius :: EnemyType -> Float
defaultRadius Aerial = 20.0
defaultRadius Ground = 20.0

-- Vida por defecto según tipo
defaultHealth :: EnemyType -> Int
defaultHealth Aerial = 50
defaultHealth Ground = 100

-------------------------------------------
-- FUNCIONES AUXILIARES
-------------------------------------------
-- normaliza un vector de dos componentes
normalize :: (Float, Float) -> (Float, Float)
normalize (0, 0) = (0, 0)
normalize (x, y) =
    let mag = sqrt (x*x + y*y)
    in if mag < 0.0001
        then (0, 0)
        else (x / mag, y / mag)

-- calcula la distancia entre dos posiciones
distance :: Position -> Position -> Float
distance (x1, y1) (x2, y2) =
    let dx = x2 - x1
        dy = y2 - y1
    in sqrt (dx*dx + dy*dy)

-------------------------------------------
-- FUNCIÓN PARA CREAR ENEMIGOS
-------------------------------------------
-- crea un nuevo enemigo con todos los parámetros
createEnemy :: EnemyID -> Int -> Position -> EnemyType -> Float -> Float -> EnemyState
createEnemy eid hp pos etype spd rad = EnemyState {
    enemy_id = eid,
    health = hp,
    position = pos,
    enemy_type = etype,
    velocity = (0, 0),
    speed = spd,
    radius = rad,
    enemyFrame = 0,
    enemyAnimTime = 0.0
}

-- Crea un enemigo con valores completamente por defecto
createEnemyDefault :: EnemyID -> Position -> EnemyType -> EnemyState
createEnemyDefault eid pos etype = 
    createEnemy eid (defaultHealth etype) pos etype enemyBaseSpeed (defaultRadius etype)

-- Inserta un nuevo enemigo en el map Enemies
addEnemy :: EnemyState -> State Enemies ()
addEnemy enemy = modify $ M.insert (enemy_id enemy) enemy

-- Método para generar enemigos dentro del game loop
spawnEnemyAt :: Position -> EnemyType -> State Enemies ()
spawnEnemyAt spawnPos etype = do
    enemies <- get
    let newID = case M.keys enemies of
                [] -> 1
                ids -> maximum ids + 1
    let newEnemy = createEnemyDefault newID spawnPos etype
    addEnemy newEnemy

-------------------------------------------
-- HITBOXES (hitbox circular)
-------------------------------------------
-- verifica si dos círculos colisionan
circlesCollide :: Position -> Float -> Position -> Float -> Bool
circlesCollide pos1 r1 pos2 r2 =
    distance pos1 pos2 <= r1 + r2

-- verifica si un enemigo colisiona con el jugador
enemyCollidesWithPlayer :: Position -> Float -> EnemyState -> Bool
enemyCollidesWithPlayer playerPos playerRadius enemy =
    circlesCollide (position enemy) (radius enemy) playerPos playerRadius

-- verifica si dos enemigos colicionan entre si
enemiesCollide :: EnemyState -> EnemyState -> Bool
enemiesCollide e1 e2 =
    circlesCollide (position e1) (radius e1) (position e2) (radius e2)

-------------------------------------------
-- RESOLVER COLISIONES
-------------------------------------------
-- separa dos enemigos que están colisionando
separateEnemies :: EnemyState -> EnemyState -> (EnemyState, EnemyState)
separateEnemies e1 e2 =
    let pos1 = position e1
        pos2 = position e2
        r1 = radius e1
        r2 = radius e2
        
        (dx, dy) = (fst pos2 - fst pos1, snd pos2 - snd pos1)
        dist = sqrt (dx*dx + dy*dy)
        
        -- Dirección normalizada
        (nx, ny) = if dist < 0.0001
                    then (1, 0)  -- Dirección arbitraria si están exactamente encima
                    else (dx / dist, dy / dist)
        
        -- Calcular overlap
        minDist = r1 + r2
        overlap = minDist - dist
        
        -- Empujar más fuerte: cada uno se mueve la mitad + un buffer
        pushDistance = (overlap / 2) + 2.0  -- Aumentamos el buffer de 0.1 a 2.0
        
        -- Nuevas posiciones
        newPos1 = (fst pos1 - nx * pushDistance, snd pos1 - ny * pushDistance)
        newPos2 = (fst pos2 + nx * pushDistance, snd pos2 + ny * pushDistance)
        
    in (e1 { position = newPos1 }, e2 { position = newPos2 })

-- Resuelve todas las colisiones entre enemigos
resolveEnemyCollisions :: State Enemies ()
resolveEnemyCollisions = do
    enemies <- get
    let enemyList = M.elems enemies
        collidingPairs = [(e1, e2) | e1 <- enemyList, 
                                    e2 <- enemyList,
                                    enemy_id e1 < enemy_id e2,
                                    enemiesCollide e1 e2]
    
    let separated = foldl separatePair enemies collidingPairs
    put separated
    where
        separatePair :: Enemies -> (EnemyState, EnemyState) -> Enemies
        separatePair m (e1, e2) =
            let (e1', e2') = separateEnemies e1 e2
            in M.insert (enemy_id e2') e2' $ M.insert (enemy_id e1') e1' m

-- Empuja enemigos lejos del jugador si colisionan
pushEnemiesAwayFromPlayer :: Position -> Float -> State Enemies ()
pushEnemiesAwayFromPlayer playerPos playerRadius = modify $ M.map pushAway
    where
        pushAway :: EnemyState -> EnemyState
        pushAway enemy =
            if enemyCollidesWithPlayer playerPos playerRadius enemy
            then
                let (ex, ey) = position enemy
                    (px, py) = playerPos
                    (dx, dy) = (ex - px, ey - py)
                    dist = sqrt (dx*dx + dy*dy)
                    
                    (nx, ny) = if dist < 0.0001
                                then (1, 0)
                                else (dx / dist, dy / dist)
                        
                    minDist = radius enemy + playerRadius
                    overlap = minDist - dist
                    pushDistance = overlap + 0.5
                    newPos = (ex + nx * pushDistance, ey + ny * pushDistance)
                    
                in enemy { position = newPos }
            else enemy

--- ANIMACION ---
-- actualizar animación de TODOS los enemigos
updateEnemyAnimations :: Float -> State GameState ()
updateEnemyAnimations dt = do
    gs <- get
    let currentEnemies = enemies gs
        updatedEnemies = M.map (updateAnim dt) currentEnemies
    put gs { enemies = updatedEnemies }
  where
    updateAnim :: Float -> EnemyState -> EnemyState
    updateAnim dt enemy =
        let newAnimTime = enemyAnimTime enemy + dt
            (newFrame, finalAnimTime) = 
                if newAnimTime >= enemyAnimationSpeed
                then ((enemyFrame enemy + 1) `mod` 2, newAnimTime - enemyAnimationSpeed)
                else (enemyFrame enemy, newAnimTime)
        in enemy { 
            enemyFrame = newFrame,
            enemyAnimTime = finalAnimTime
        }