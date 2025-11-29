module Types where

import Graphics.Gloss
import Data.Ix (Ix)
import qualified Data.Map.Strict as Map
import MapLoader (CollisionShape)

-- Tipos básicos
type Position = (Float, Float)
type Velocity = (Float, Float)
type TileCoord = (Int, Int)

-- Tipos específicos
data Direction = DirDown | DirRight | DirUp | DirLeft 
    deriving (Eq, Ord, Show, Ix, Bounded)

data AnimType = Idle | Walk 
    deriving (Eq, Ord, Show, Ix, Bounded)

data ItemType = Ballesta | Boomerang | Espada | Curacion | Velocidad | Stamina | Fuerza
    deriving (Eq, Ord, Show, Ix, Bounded)

data BoomerangState = Flying | Returning
    deriving (Eq, Show)


-- Proyectil (Flechas de la ballesta)
data Projectile = Projectile {
    projPos :: Position,
    projVel :: Velocity,
    projLifetime :: Float
} deriving (Show)


-- Proyectil boomerang
data BoomerangProjectile = BoomerangProjectile {
    boomerangPos :: Position,
    boomerangVel :: Velocity,
    boomerangState :: BoomerangState,
    boomerangDistanceTraveled :: Float,
    boomerangRotation :: Float,             -- Ángulo de rotación actual
    boomerangInitialDir :: (Float, Float)   -- Dirección inicial en la que fue disparado
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
    playerCooldownBallesta :: Float,
    playerHasBoomerang :: Bool
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
    boomerang :: Maybe BoomerangProjectile,   -- Solo habrá un boomerang a la vez
    worldItems :: [WorldItem],
    inputState :: InputState,
    tileMap :: [[Int]],
    allLayers :: [[[Int]]],  -- Todas las capas del mapa (para colisiones)
    collisionMap :: [[Bool]],
    collisionShapes :: Map.Map Int [CollisionShape],  -- GID -> shapes de colisión
    randomSeed :: Int
} deriving (Show)


-- Constantes

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
projectileSpeed :: Float
projectileSpeed = 1200

projectileLifetime :: Float
projectileLifetime = 1.5

-- DAÑO QUE VA A INFLINGIR LA FLECHA
arrowDamage :: Float
arrowDamage = 60 -- suponiendo que el enemigo tenga 100 de vida

-- Daño que va a inflingir el boomerang
boomerangDamage :: Float
boomerangDamage = 40

-- Daño que va a inflingir la espada
swordDamage :: Float
swordDamage = 30

cooldownBallesta :: Float
cooldownBallesta = 0.8

cooldownBarWidth :: Float
cooldownBarWidth = 50.0

cooldownBarHeight :: Float
cooldownBarHeight = 4.0 

-- Boomerang
boomerangSpeed :: Float         -- Velocidad de tiro del boomerang
boomerangSpeed = 800.0          

boomerangMaxDistance :: Float   -- Distancia máxima de tiro del boomerang
boomerangMaxDistance = 400      -- Pixeles

boomerangSpinSpeed :: Float     -- Velocidad de giro del boomerang
boomerangSpinSpeed = 1080        -- grados por segundo

boomerangReturnAccel :: Float   -- Aceleración del boomerang al regresar
boomerangReturnAccel = 1000.0

boomerangCatchRadius :: Float   -- Radio para atrapar al boomerang
boomerangCatchRadius = 40.0


------------------- JUGADOR -------------------
playerBaseSpeed :: Float
playerBaseSpeed = 350

playerSprintSpeed :: Float
playerSprintSpeed = 500

playerBaseHealth :: Int
playerBaseHealth = 100

playerCollisionHalfSize :: Float
playerCollisionHalfSize = 14.0  -- Mitad del tamaño de colisión del jugador (28x28 píxeles)

-- Offset Y de la colisión del jugador (negativo = más abajo, hacia los pies)
playerCollisionOffsetY :: Float
playerCollisionOffsetY = -20.0  -- Bajar la colisión hacia los pies del sprite



-- Nombre de cada item en String
itemName :: ItemType -> String
itemName Ballesta = "Ballesta"
itemName Boomerang = "Boomerang"
itemName Espada = "Espada"
itemName Curacion = "Pocion de curacion"
itemName Velocidad = "Pocion de velocidad"
itemName Stamina = "Pocion de stamina"
itemName Fuerza = "Pocion de fuerza"