module Types where
import Graphics.Gloss
import Data.Ix (Ix)
import qualified Data.Map.Strict as Map
import MapLoader (CollisionShape)

-- Tipos básicos
type Position = (Float, Float)
type Velocity = (Float, Float)
type TileCoord = (Int, Int)

data Direction = DirDown | DirRight | DirUp | DirLeft 
    deriving (Eq, Ord, Show, Ix, Bounded)

data AnimType = Idle | Walk 
    deriving (Eq, Ord, Show, Ix, Bounded)

data ItemType = Ballesta | Boomerang | Espada | Curacion | Velocidad | Stamina | Fuerza
    deriving (Eq, Ord, Show, Ix, Bounded)

-- NUEVO: Objeto destructible
data DestructibleObject = DestructibleObject {
    destPos :: Position,          -- Posición del objeto
    destHealth :: Float,          -- Vida actual
    destMaxHealth :: Float,       -- Vida máxima
    destGid :: Int,               -- GID del tile de colisión
    destTilePos :: TileCoord      -- Posición (col, row) del tile
} deriving (Show, Eq)

-- Proyectil
data Projectile = Projectile {
    projPos :: Position,
    projVel :: Velocity,
    projLifetime :: Float
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
    playerEquippedItem :: Maybe ItemType,
    playerCooldownBallesta :: Float
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
    keyW :: Bool,
    keyA :: Bool,
    keyS :: Bool,
    keyD :: Bool,
    keyB :: Bool,
    keyE :: Bool,
    mousePos :: Position,
    mouseClick :: Bool
} deriving (Show)

-- Estado del juego
data GameState = GameState {
    player :: Player,
    camera :: Camera,
    projectiles :: [Projectile],
    worldItems :: [WorldItem],
    destructibleObjects :: [DestructibleObject],
    inputState :: InputState,
    tileMap :: [[Int]],
    allLayers :: [[[Int]]],
    collisionMap :: [[Bool]],
    collisionShapes :: Map.Map Int [CollisionShape],
    randomSeed :: Int
} deriving (Show)

-- Constantes
tileSize :: Float
tileSize = 64

screenWidth :: Int
screenWidth = 1280

screenHeight :: Int
screenHeight = 720

projectileSpeed :: Float
projectileSpeed = 1200

projectileLifetime :: Float
projectileLifetime = 1.5

arrowDamage :: Float
arrowDamage = 35.0

boomerangDamage :: Float
boomerangDamage = 40

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

cameraSmoothing :: Float
cameraSmoothing = 0.15

itemPickupRadius :: Float
itemPickupRadius = 50.0

itemFloatSpeed :: Float
itemFloatSpeed = 2.0

itemFloatHeight :: Float
itemFloatHeight = 8.0

cooldownBallesta :: Float
cooldownBallesta = 0.8

cooldownBarWidth :: Float
cooldownBarWidth = 50.0

cooldownBarHeight :: Float
cooldownBarHeight = 4.0

-- CONFIGURACIÓN DE OBJETOS DESTRUCTIBLES
-- GIDs de los objetos destructibles (ids de colisión)
destructibleGids :: [Int]
destructibleGids = [833 + 85, 833 + 21, 1665 + 149]  -- [918, 854, 1814]

-- Vida máxima según GID
getMaxHealth :: Int -> Float
getMaxHealth 918 = 70.0   -- Barril (85 + 833)
getMaxHealth 854 = 105.0  -- Caja grande (21 + 833)  
getMaxHealth 1814 = 70.0  -- Vasija (149 + 1665)
getMaxHealth _ = 100.0

-- Item que dropea cada objeto
getLootItem :: Int -> ItemType
getLootItem 918 = Curacion    -- Barril -> Poción de curación
getLootItem 854 = Fuerza      -- Caja -> Poción de fuerza  
getLootItem 1814 = Velocidad  -- Vasija -> Poción de velocidad
getLootItem _ = Curacion

-- Nombre de cada item
itemName :: ItemType -> String
itemName Ballesta = "Ballesta"
itemName Boomerang = "Boomerang"
itemName Espada = "Espada"
itemName Curacion = "Pocion de curacion"
itemName Velocidad = "Pocion de velocidad"
itemName Stamina = "Pocion de stamina"
itemName Fuerza = "Pocion de fuerza"