import Control.Monad.State
-- para guardar los enemigos
import qualified Data.Map as M

-------------------------------------------
-- DEFINICIONES BASE
-------------------------------------------

-- estas tres cosas no lo entiendo lol
type Position = (Float, Float)
type Velocity = (Float, Float)
type TileCoord = (Int, Int)
-- id del enemigo
type EnemyID = Int

-- def. del tipo del enemigo
data EnemyType = Aerial | Ground deriving (Show, Eq)

-- definición de enemigo
data EnemyState = EnemyState{
    enemy_id:: EnemyID,
    health:: Int,
    position:: Position,
    enemy_type:: EnemyType,
    velocity:: Velocity,
    speed :: Float,
    radius :: Float
} deriving (Show)

-- estado global que contiene un map de enemigos
type Enemies = M.Map EnemyID EnemyState

-------------------------------------------
-- OPERACIONES PARA EL STATE DE UN ENEMY
-------------------------------------------

-- daño que recibe el enemigo
damageReceived :: EnemyID -> Int -> State Enemies ()
damageReceived eID dmg = modify $ \m ->
    case M.lookup eID m of
        Nothing -> m  -- si no existe, no hacemos nada
        Just enemy ->
            let newHealth = health enemy - dmg
            in if newHealth <= 0
                then M.delete eID m  -- enemigo muere
                else M.insert eID (enemy { health = newHealth }) m

-------------------------------------------
-- TRACKEO
-------------------------------------------

-- trackPlayer de un enemigo individual
trackPlayer :: Position -> EnemyID -> State Enemies ()
trackPlayer playerPos eID = modify $ \m ->
    case M.lookup eID m of
        Nothing     -> m  -- enemigo no existe (muerto)
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
enemyBaseSpeed = 300

-- radio por defecto según tipo de enemigo
defaultRadius :: EnemyType -> Float
defaultRadius Aerial = 15.0
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
    in if mag < 0.0001  -- epsilon para evitar divisiones problemáticas
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
    radius = rad
}

-- Crea un enemigo con valores completamente por defecto
createEnemyDefault :: EnemyID -> Position -> EnemyType -> EnemyState
createEnemyDefault eid pos etype = 
    createEnemy eid (defaultHealth etype) pos etype enemyBaseSpeed (defaultRadius etype)

-- Inserta un nuevo enemigo en el map Enemies
addEnemy :: EnemyState -> State Enemies ()
addEnemy enemy = modify $ M.insert (enemy_id enemy) enemy
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
-- Separa dos enemigos que están colisionando
separateEnemies :: EnemyState -> EnemyState -> (EnemyState, EnemyState)
separateEnemies e1 e2 =
    let pos1 = position e1
        pos2 = position e2
        r1 = radius e1
        r2 = radius e2
        
        -- Dirección de e1 hacia e2
        (dx, dy) = (fst pos2 - fst pos1, snd pos2 - snd pos1)
        dist = sqrt (dx*dx + dy*dy)
        
        -- Si están exactamente en la misma posición, empuja uno arbitrariamente
        (nx, ny) = if dist < 0.0001
                    then (1, 0)
                    else (dx / dist, dy / dist)
        
        -- Distancia de overlap
        overlap = r1 + r2 - dist
        
        -- Cada enemigo se mueve la mitad del overlap
        pushDistance = overlap / 2 + 0.1  -- +0.1 para asegurar separación
        
        -- Nueva posición de e1 (empujado hacia atrás)
        newPos1 = (fst pos1 - nx * pushDistance, snd pos1 - ny * pushDistance)
        
        -- Nueva posición de e2 (empujado hacia adelante)
        newPos2 = (fst pos2 + nx * pushDistance, snd pos2 + ny * pushDistance)
        
    in (e1 { position = newPos1 }, e2 { position = newPos2 })

-- Resuelve todas las colisiones entre enemigos
resolveEnemyCollisions :: State Enemies ()
resolveEnemyCollisions = do
    enemies <- get
    let enemyList = M.elems enemies
        -- Encuentra todos los pares que colisionan
        collidingPairs = [(e1, e2) | e1 <- enemyList, 
                                    e2 <- enemyList,
                                    enemy_id e1 < enemy_id e2,  -- evita duplicados
                                    enemiesCollide e1 e2]
    
    -- Separa cada par que colisiona
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
                    -- Dirección del jugador al enemigo
                    (dx, dy) = (ex - px, ey - py)
                    dist = sqrt (dx*dx + dy*dy)
                    
                    (nx, ny) = if dist < 0.0001
                                then (1, 0)  -- dirección arbitraria si están encima
                                else (dx / dist, dy / dist)
                        
                    -- Distancia que deben estar separados
                    minDist = radius enemy + playerRadius
                    overlap = minDist - dist
                    
                    -- Nueva posición del enemigo (empujado lejos del jugador)
                    pushDistance = overlap + 0.5  -- +0.5 para buffer
                    newPos = (ex + nx * pushDistance, ey + ny * pushDistance)
                    
                in enemy { position = newPos }
            else enemy
        
-- Encuentra todos los enemigos que están tocando al jugador
getEnemiesCollidingWithPlayer :: Position -> Float -> State Enemies [EnemyID]
getEnemiesCollidingWithPlayer playerPos playerRadius = do
    enemies <- get
    return [enemy_id e | e <- M.elems enemies, 
            enemyCollidesWithPlayer playerPos playerRadius e]
