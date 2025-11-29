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

data ItemType = Ballesta | Boomerang | Espada
    deriving (Eq, Ord, Show, Ix, Bounded)

-- Proyectil
data Projectile = Projectile {
    projPos :: Position,
    projVel :: Velocity,
    projLifetime :: Float
} deriving (Show)


-- Jugador
data Player = Player {
    playerPos :: Position,
    playerVel :: Velocity,
    playerHealth :: Int,
    playerSpeed :: Float,
    playerDir :: Direction,
    playerFrame :: Int,
    playerAnimTime :: Float,
    playerEquippedItem :: Maybe ItemType
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
    inputState :: InputState,
    tileMap :: [[Int]],
    allLayers :: [[[Int]]],  -- Todas las capas del mapa (para colisiones)
    collisionMap :: [[Bool]],
    collisionShapes :: Map.Map Int [CollisionShape],  -- GID -> shapes de colisión
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
projectileSpeed = 800

projectileLifetime :: Float
projectileLifetime = 1.5

playerBaseSpeed :: Float
playerBaseSpeed = 350

playerSprintSpeed :: Float
playerSprintSpeed = 550

playerBaseHealth :: Int
playerBaseHealth = 100

playerCollisionHalfSize :: Float
playerCollisionHalfSize = 14.0  -- Mitad del tamaño de colisión del jugador (28x28 píxeles)

-- Offset Y de la colisión del jugador (negativo = más abajo, hacia los pies)
playerCollisionOffsetY :: Float
playerCollisionOffsetY = -20.0  -- Bajar la colisión hacia los pies del sprite

cameraSmoothing :: Float
cameraSmoothing = 0.15

itemPickupRadius :: Float
itemPickupRadius = 50.0

itemFloatSpeed :: Float
itemFloatSpeed = 2.0

itemFloatHeight :: Float
itemFloatHeight = 8.0

-- Nombre de cada item en String
itemName :: ItemType -> String
itemName Ballesta = "Ballesta"
itemName Boomerang = "Boomerang"
itemName Espada = "Espada"