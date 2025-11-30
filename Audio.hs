module Audio where

import SDL.Mixer qualified as Mix
import SDL qualified
import Control.Monad (void, when)
import Control.Exception (try, SomeException)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

-- estos dos de arriba faltan, y las ultimas dos armas

menuMusic :: IORef (Maybe Mix.Music)
menuMusic = unsafePerformIO $ newIORef Nothing
{-# NOINLINE menuMusic #-}

victoryMusic :: IORef (Maybe Mix.Music)
victoryMusic = unsafePerformIO $ newIORef Nothing
{-# NOINLINE victoryMusic #-}

-- de acá para abajo de esta seccion están listos

defeatMusic :: IORef (Maybe Mix.Music)
defeatMusic = unsafePerformIO $ newIORef Nothing
{-# NOINLINE defeatMusic #-}

gameMusic :: IORef (Maybe Mix.Music)
gameMusic = unsafePerformIO $ newIORef Nothing
{-# NOINLINE gameMusic #-}

takeItem :: IORef (Maybe Mix.Music)
takeItem = unsafePerformIO $ newIORef Nothing
{-# NOINLINE defeatMusic #-}

powerUpFX :: IORef (Maybe Mix.Music)
powerUpFX = unsafePerformIO $ newIORef Nothing
{-# NOINLINE defeatMusic #-}

playerStepFX :: IORef (Maybe Mix.Music)
playerStepFX = unsafePerformIO $ newIORef Nothing
{-# NOINLINE defeatMusic #-}

playerHitFX :: IORef (Maybe Mix.Music)
playerHitFX = unsafePerformIO $ newIORef Nothing
{-# NOINLINE defeatMusic #-}

-------
-- armas (solo está el de la ballesta)
-------
ballestaFX :: IORef (Maybe Mix.Music)
ballestaFX = unsafePerformIO $ newIORef Nothing
{-# NOINLINE defeatMusic #-}

swordFX :: IORef (Maybe Mix.Music)
swordFX = unsafePerformIO $ newIORef Nothing
{-# NOINLINE defeatMusic #-}

boomerangFX :: IORef (Maybe Mix.Music)
boomerangFX = unsafePerformIO $ newIORef Nothing
{-# NOINLINE defeatMusic #-}