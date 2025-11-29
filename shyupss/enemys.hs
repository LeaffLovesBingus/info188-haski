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
-- def. de mapa para enemigos in game
type EnemyMap = M.Map EnemyID EnemyState
-- def. del tipo del enemigo
data EnemyType = Aerial | Ground | UnderGround deriving (Show, Eq)

-- definición de enemigo
data EnemyState = EnemyState{
    enemy_id:: EnemyID,
    health:: Int,
    position:: Position,
    enemy_type:: EnemyType,
    velocity:: Velocity
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
                then M.delete eID m        -- enemigo muere
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
trackAllEnemies playerPos = do
    enemyIDs <- gets M.keys
    mapM_ (trackPlayer playerPos) enemyIDs

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
updateAllEnemyMovement dt = do
    ids <- gets M.keys
    mapM_ (updateEnemyMovement dt) ids

-------------------------------------------
-- CONSTANTES
-------------------------------------------
-- speed BASE para enemigos
enemyBaseSpeed :: Float
enemyBaseSpeed = 300

-------------------------------------------
-- FUNCIONES AUXILIARES
-------------------------------------------
normalize :: (Float, Float) -> (Float, Float)
normalize (0,0) = (0,0)
normalize (x,y) =
    let mag = sqrt (x*x + y*y)
    in (x / mag, y / mag)

-------------------------------------------
-- EN EL CICLO DEL JUEGO SE DEBE HACER:
-- trackAllEnemies playerPosition
-- updateAllEnemyMovement deltaTime
-------------------------------------------