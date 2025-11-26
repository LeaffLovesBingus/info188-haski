module Types where

import Graphics.Gloss
import Data.Ix (Ix)


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
    cameraPos :: Position
} deriving (Show)


-- Input del usuario
data InputState = InputState {
    keyW :: Bool,
    keyA :: Bool,
    keyS :: Bool,
    keyD :: Bool,
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
projectileSpeed = 1000

projectileLifetime :: Float
projectileLifetime = 3

playerBaseSpeed :: Float
playerBaseSpeed = 350

playerBaseHealth :: Int
playerBaseHealth = 100