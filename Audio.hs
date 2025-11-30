module Audio where

import SDL.Mixer qualified as Mix
import SDL qualified
import Control.Monad (void, when)
import Control.Exception (try, SomeException)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

------------------- CANCIONES -------------------
menuTheme :: IORef (Maybe Mix.Music)
menuTheme = unsafePerformIO $ newIORef Nothing
{-# NOINLINE menuTheme #-}

gameTheme :: IORef (Maybe Mix.Music)
gameTheme = unsafePerformIO $ newIORef Nothing
{-# NOINLINE gameTheme #-}

victoryTheme :: IORef (Maybe Mix.Music)
victoryTheme = unsafePerformIO $ newIORef Nothing
{-# NOINLINE victoryTheme #-}

defeatTheme :: IORef (Maybe Mix.Music)
defeatTheme = unsafePerformIO $ newIORef Nothing
{-# NOINLINE defeatTheme #-}

------------------- EFECTOS DE SONIDO -------------------
takeItemFX :: IORef (Maybe Mix.Chunk)
takeItemFX = unsafePerformIO $ newIORef Nothing
{-# NOINLINE takeItemFX #-}

playerHitFX :: IORef (Maybe Mix.Chunk)
playerHitFX = unsafePerformIO $ newIORef Nothing
{-# NOINLINE playerHitFX #-}

playerStepFX :: IORef (Maybe Mix.Chunk)
playerStepFX = unsafePerformIO $ newIORef Nothing
{-# NOINLINE playerStepFX #-}

ballestaDisparoFX :: IORef (Maybe Mix.Chunk)
ballestaDisparoFX = unsafePerformIO $ newIORef Nothing
{-# NOINLINE ballestaDisparoFX #-}

ballestaCargaFX :: IORef (Maybe Mix.Chunk)
ballestaCargaFX = unsafePerformIO $ newIORef Nothing
{-# NOINLINE ballestaCargaFX #-}

boomerangTravelFX :: IORef (Maybe Mix.Chunk)
boomerangTravelFX = unsafePerformIO $ newIORef Nothing
{-# NOINLINE boomerangTravelFX #-}

boomerangHitFX :: IORef (Maybe Mix.Chunk)
boomerangHitFX = unsafePerformIO $ newIORef Nothing
{-# NOINLINE boomerangHitFX #-}

powerUpFX :: IORef (Maybe Mix.Chunk)
powerUpFX = unsafePerformIO $ newIORef Nothing
{-# NOINLINE powerUpFX #-}

swordSlashFX :: IORef (Maybe Mix.Chunk)
swordSlashFX = unsafePerformIO $ newIORef Nothing
{-# NOINLINE swordSlashFX #-}


------------------- FUNCIONES AUXILIARES -------------------
-- Función para cargar todos los audios
loadAllAudio :: IO ()
loadAllAudio = do
    -- Cargar música de fondo
    loadMusic menuTheme "sounds_fx/menu-theme.wav"
    loadMusic gameTheme "sounds_fx/game-theme.wav"
    loadMusic victoryTheme "sounds_fx/victory-theme.wav"
    loadMusic defeatTheme "sounds_fx/defeat-theme.wav"
    
    -- Cargar efectos de sonido
    loadChunk takeItemFX "sounds_fx/takeitem.wav"
    loadChunk playerHitFX "sounds_fx/player-hit.wav"
    loadChunk playerStepFX "sounds_fx/player-step.wav"
    loadChunk ballestaDisparoFX "sounds_fx/ballesta-disparo.wav"
    loadChunk ballestaCargaFX "sounds_fx/ballesta-carga.wav"
    loadChunk boomerangTravelFX "sounds_fx/boomerang-hit.wav"
    loadChunk boomerangHitFX "sounds_fx/boomerang-travel.wav"
    loadChunk powerUpFX "sounds_fx/powerup.wav"
    loadChunk swordSlashFX "sounds_fx/sword-fx.wav"
    
    putStrLn "Audio cargado exitosamente"

-- Cargar música
loadMusic :: IORef (Maybe Mix.Music) -> FilePath -> IO ()
loadMusic ref path = do
    result <- try (Mix.load path) :: IO (Either SomeException Mix.Music)
    case result of
        Right music -> do
            writeIORef ref (Just music)
            putStrLn $ "Cargado: " ++ path
        Left err -> putStrLn $ "Error cargando " ++ path ++ ": " ++ show err

-- Cargar chunk (efecto de sonido)
loadChunk :: IORef (Maybe Mix.Chunk) -> FilePath -> IO ()
loadChunk ref path = do
    result <- try (Mix.load path) :: IO (Either SomeException Mix.Chunk)
    case result of
        Right chunk -> do
            writeIORef ref (Just chunk)
            putStrLn $ "Cargado: " ++ path
        Left err -> putStrLn $ "Error cargando " ++ path ++ ": " ++ show err

-- Reproducir música en loop
playMusicLoop :: IORef (Maybe Mix.Music) -> IO ()
playMusicLoop ref = do
    musicMaybe <- readIORef ref
    case musicMaybe of
        Just music -> Mix.playMusic Mix.Forever music
        Nothing -> return ()

-- Detener música
stopMusic :: IO ()
stopMusic = Mix.haltMusic

-- Reproducir efecto de sonido
playSoundFX :: IORef (Maybe Mix.Chunk) -> IO ()
playSoundFX ref = do
    chunkMaybe <- readIORef ref
    case chunkMaybe of
        Just chunk -> void $ Mix.play chunk
        Nothing -> return ()

-- Reproducir efecto con volumen específico (0-128)
playSoundFXVolume :: IORef (Maybe Mix.Chunk) -> Int -> IO ()
playSoundFXVolume ref volume = do
    chunkMaybe <- readIORef ref
    case chunkMaybe of
        Just chunk -> do
            Mix.setVolume (min 128 $ max 0 volume) chunk
            void $ Mix.play chunk
        Nothing -> return ()