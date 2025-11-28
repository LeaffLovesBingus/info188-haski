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
    playerAnimTime :: Float
} deriving (Show)


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
    mousePos :: Position,
    mouseClick :: Bool
} deriving (Show)


-- Estado del juego
data GameState = GameState {
    player :: Player,
    camera :: Camera,
    projectiles :: [Projectile],
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
playerCollisionHalfSize = 12.0  -- Mitad del tamaño de colisión del jugador (40x40 píxeles, escalado con sprite 3x)

cameraSmoothing :: Float
cameraSmoothing = 0.15