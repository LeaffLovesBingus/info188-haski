module Types where
import Graphics.Gloss
import Data.Ix (Ix)
import qualified Data.Map.Strict as Map
import MapLoader (CollisionShape)

-- Tipos básicos
type Position = (Float, Float)
type Velocity = (Float, Float)
type TileCoord = (Int, Int)

-- Escenas del juego
data GameScene = MenuScreen | Playing | Victory | Defeat
    deriving (Eq, Show)

-- Tipos específicos
data Direction = DirDown | DirRight | DirUp | DirLeft 
    deriving (Eq, Ord, Show, Ix, Bounded)

data AnimType = Idle | Walk | Damage 
    deriving (Eq, Ord, Show, Ix, Bounded)

data ItemType = Ballesta | Boomerang | Espada | Curacion | Velocidad | Stamina | Fuerza
    deriving (Eq, Ord, Show, Ix, Bounded)

data DamageDirection = DamageFromFront | DamageFromBack | DamageFromLeft | DamageFromRight
    deriving (Eq, Show)

-- Objeto destructible
data DestructibleObject = DestructibleObject {
    destPos :: Position,          -- Posición del objeto
    destHealth :: Float,          -- Vida actual
    destMaxHealth :: Float,       -- Vida máxima
    destGid :: Int,               -- GID del tile de colisión
    destTilePos :: TileCoord      -- Posición (col, row) del tile
} deriving (Show, Eq)

-- Proyectil
data BoomerangState = Flying | Returning
    deriving (Eq, Show)

data FlashState = NoFlash | Showing ItemType | FadingOut ItemType Float
    deriving (Eq, Show)

-- Proyectil (Flechas de la ballesta)
data Projectile = Projectile {
    projPos :: Position,
    projVel :: Velocity,
    projLifetime :: Float
} deriving (Show, Eq)

-- Proyectil boomerang
data BoomerangProjectile = BoomerangProjectile {
    boomerangPos :: Position,
    boomerangVel :: Velocity,
    boomerangState :: BoomerangState,
    boomerangDistanceTraveled :: Float,
    boomerangRotation :: Float,             -- Ángulo de rotación actual
    boomerangInitialDir :: (Float, Float)   -- Dirección inicial en la que fue disparado
} deriving (Show, Eq)

-- Slash de espada
data SwordSlash = SwordSlash {
    slashPos :: Position,
    slashAngle :: Float,
    slashFrame :: Int,
    slashTimer :: Float,
    slashActive :: Bool,
    slashHitEnemies :: [EnemyID]
} deriving (Show, Eq)

-- Jugador
data Player = Player {
    playerPos :: Position,
    playerVel :: Velocity,
    playerHealth :: Int,
    playerSpeed :: Float,
    playerDir :: Direction,
    playerFrame :: Int,
    playerAnimTime :: Float,
    playerCooldownBallesta :: Float,
    playerHasBoomerang :: Bool,
    playerEquippedItem :: Maybe ItemType,
    playerInventory :: [Maybe ItemType],
    playerSelectedSlot :: Int,
    playerItemFlashTimer :: Float,
    playerItemFlashState :: FlashState,
    playerSpeedBoostTimer :: Float,
    playerStrengthBoostTimer :: Float,  -- Timer para poción de fuerza
    playerIsTakingDamage :: Bool,
    playerDamageAnimTimer :: Float,
    playerDamageDirection :: DamageDirection,
    playerDamageKnockbackVel :: Velocity,
    playerIsInvulnerable :: Bool
} deriving (Show)

-- Item en el mundo
data WorldItem = WorldItem {
    itemPos :: Position,
    itemType :: ItemType,
    itemFloatTime :: Float
} deriving (Show, Eq)

-- Cámara
data Camera = Camera {
    cameraPos :: Position,
    cameraTarget :: Position
} deriving (Show)

-- Input del usuario
data InputState = InputState {
    keyW :: Bool,           -- Adelante
    keyA :: Bool,           -- Izquierda
    keyS :: Bool,           -- Atrás
    keyD :: Bool,           -- Derecha
    keyShift :: Bool,           -- Sprint
    keyE :: Bool,           -- Recoger item
    keyF :: Bool,           -- Tirar item
    key1 :: Bool,           -- Slot 1
    key2 :: Bool,           -- Slot 2
    key3 :: Bool,           -- Slot 3
    key4 :: Bool,           -- Slot 4
    key5 :: Bool,           -- Slot 5
    mousePos :: Position,   -- Posición del mouse
    mouseClick :: Bool      -- Disparar/consumir
} deriving (Show)

-- Estado del juego
data GameState = GameState {
    currentScene :: GameScene,
    gameTimer :: Float,                       -- Temporizador del juego (segundos restantes)
    player :: Player,
    camera :: Camera,
    projectiles :: [Projectile],
    boomerang :: Maybe BoomerangProjectile,   -- Solo habra un boomerang a la vez
    swordSlash :: Maybe SwordSlash,           -- Solo habra un espadazo a la vez
    worldItems :: [WorldItem],
    destructibleObjects :: [DestructibleObject],
    inputState :: InputState,
    tileMap :: [[Int]],
    allLayers :: [[[Int]]],
    collisionMap :: [[Bool]],
    collisionShapes :: Map.Map Int [CollisionShape],
    randomSeed :: Int,
    enemies:: Enemies,
    enemyRespawnTimer :: Float,               -- Timer para respawnear enemigos
    nextEnemyId :: Int,                       -- ID para el próximo enemigo
    exitRequested :: Bool,                    -- Flag para cerrar el juego
    victoryTriggered :: Bool,
    defeatTriggered :: Bool
} deriving (Show)

-- Tiempo para ganar (3 minutos = 180 segundos)
gameWinTime :: Float
gameWinTime = 180.0

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
    radius :: Float,
    enemyFrame :: Int, -- Frame actual (0 o 1)
    enemyAnimTime :: Float -- tiempo de animacion
} deriving (Show)

-- estado global que contiene un map de enemigos
type Enemies = Map.Map EnemyID EnemyState

------------------- PANTALLA -------------------
screenWidth :: Int
screenWidth = 1280

screenHeight :: Int
screenHeight = 720

cameraSmoothing :: Float
cameraSmoothing = 0.15


------------------- PARTIDA -------------------
tileSize :: Float
tileSize = 64

itemPickupRadius :: Float
itemPickupRadius = 50.0

itemFloatSpeed :: Float
itemFloatSpeed = 2.0

itemFloatHeight :: Float
itemFloatHeight = 8.0


------------------- ARMAS -------------------
-- Ballesta
projectileSpeed :: Float
projectileSpeed = 1200

projectileLifetime :: Float
projectileLifetime = 1.5

arrowDamage :: Float
arrowDamage = 50.0

cooldownBallesta :: Float
cooldownBallesta = 0.8

cooldownBarWidth :: Float
cooldownBarWidth = 50.0

cooldownBarHeight :: Float
cooldownBarHeight = 4.0 

-- Boomerang
boomerangDamage :: Float
boomerangDamage = 15.0

boomerangSpeed :: Float         -- Velocidad de tiro del boomerang
boomerangSpeed = 800.0          

boomerangMaxDistance :: Float   -- Distancia máxima de tiro del boomerang
boomerangMaxDistance = 300      -- Pixeles

boomerangSpinSpeed :: Float     -- Velocidad de giro del boomerang
boomerangSpinSpeed = 1080        -- grados por segundo

boomerangReturnAccel :: Float   -- Aceleración del boomerang al regresar
boomerangReturnAccel = 1000.0

boomerangCatchRadius :: Float   -- Radio para atrapar al boomerang
boomerangCatchRadius = 20.0

-- Espada
swordDamage :: Float
swordDamage = 50.0

swordSlashRadius :: Float
swordSlashRadius = 70.0

slashAnimationDuration :: Float
slashAnimationDuration = 0.2

slashFrameCount :: Int
slashFrameCount = 6

slashOffset :: Float
slashOffset = 40.0

------------------- JUGADOR -------------------
playerBaseSpeed :: Float
playerBaseSpeed = 350

playerSprintSpeed :: Float
playerSprintSpeed = 500

playerBaseHealth :: Int
playerBaseHealth = 100

playerCollisionHalfSize :: Float
playerCollisionHalfSize = 14.0

playerCollisionOffsetY :: Float
playerCollisionOffsetY = -20.0

damageAnimationDuration :: Float
damageAnimationDuration = 0.3

damageKnockbackDistance :: Float
damageKnockbackDistance = 600.0

playerRadius:: Float
playerRadius = 15.0

------------------- POCIONES -------------------
-- Poción de velocidad
speedBoostDuration :: Float
speedBoostDuration = 15.0 -- Duración en segundos del boost de velocidad

potionSpeedBoost :: Float
potionSpeedBoost = 200.0

-- Poción de curación
healAmount :: Int
healAmount = 40

-- Poción de fuerza
strengthBoostDuration :: Float
strengthBoostDuration = 10.0  -- Duración del boost de fuerza

strengthDamageMultiplier :: Float
strengthDamageMultiplier = 2.0  -- Multiplicador de daño con poción de fuerza

------------------- CONFIGURACIÓN DE OBJETOS DESTRUCTIBLES -------------------
-- GIDs de los objetos destructibles (ids de colisión)
destructibleGids :: [Int]
destructibleGids = [1665 + 85, 1665 + 21, 1665 + 149]  -- [1750, 1686, 1814]

-- Vida máxima según GID
getMaxHealth :: Int -> Float
getMaxHealth 1750 = 70.0   -- Barril (85 + 1665)
getMaxHealth 1686 = 105.0  -- Caja grande (21 + 1665)  
getMaxHealth 1814 = 70.0  -- Vasija (149 + 1665)
getMaxHealth _ = 100.0

-- TODOS los GIDs que componen cada objeto destructible
getAllDestructibleGids :: Int -> [Int]
getAllDestructibleGids 1750 = [1734, 1750, 1751]  -- Barril completo
getAllDestructibleGids 1686 = [1670, 1671, 1686, 1687]  -- Caja completa
getAllDestructibleGids 1814 = [1814]  -- Vasija
getAllDestructibleGids _ = []

-- Offsets relativos CORREGIDOS para cada objeto: (offset_col, offset_row)
getDestructibleOffsets :: Int -> [(Int, Int)]
getDestructibleOffsets 1750 = [  -- Barril (estructura en L)
    (0, -1),  -- Tile superior (arriba del de colisión)
    (0, 0),   -- Tile de colisión (posición base)
    (1, 0)    -- Tile de sombra (derecha del de colisión)
    ]
getDestructibleOffsets 1686 = [  -- Caja (2x2 completo)
    (0, -1),  -- Tile superior izquierdo (arriba-izquierda)
    (1, -1),  -- Tile superior derecho (arriba-derecha)
    (0, 0),   -- Tile de colisión izquierdo (posición base)
    (1, 0)    -- Tile de sombra derecho (derecha del de colisión)
    ]
getDestructibleOffsets 1814 = [  -- Vasija
    (0, 0)    -- Solo un tile
    ]
getDestructibleOffsets _ = [(0, 0)]

-- Item que dropea cada objeto
getLootItem :: Int -> ItemType
getLootItem 1750 = Curacion    -- Barril -> Poción de curación
getLootItem 1686 = Fuerza      -- Caja -> Poción de fuerza  
getLootItem 1814 = Velocidad  -- Vasija -> Poción de velocidad
getLootItem _ = Curacion


------------------- INVENTARIO -------------------
inventorySize :: Int
inventorySize = 5

itemNameFlashDuration :: Float
itemNameFlashDuration = 0.5

itemNameFadeOutDuration :: Float
itemNameFadeOutDuration = 0.5

itemNameFlashYOffset :: Float   -- Offset en el eje Y sobre el jugador
itemNameFlashYOffset = 40.0


-- Nombre de cada item en String
itemName :: ItemType -> String
itemName Ballesta = "Ballesta"
itemName Boomerang = "Boomerang"
itemName Espada = "Espada"
itemName Curacion = "Pocion de curacion"
itemName Velocidad = "Pocion de velocidad"
itemName Stamina = "Pocion de stamina"
itemName Fuerza = "Pocion de fuerza"

------------------- ENEMIGOS -------------------
dmgFromEnemy:: Int
dmgFromEnemy = 10

enemyAnimationSpeed :: Float
enemyAnimationSpeed = 0.3  -- Cambia de frame cada 0.3 segundos

-- Constantes de respawn de enemigos
maxEnemies :: Int
maxEnemies = 5  -- Máximo de enemigos en el mapa

enemyRespawnDelay :: Float
enemyRespawnDelay = 3.0  -- Segundos antes de respawnear un enemigo