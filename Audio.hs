module Audio where

import SDL.Mixer qualified as Mix
import SDL qualified
import Control.Monad (void, when)
import Control.Exception (try, SomeException)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

menuMusic :: IORef (Maybe Mix.Music)
menuMusic = unsafePerformIO $ newIORef Nothing
{-# NOINLINE menuMusic #-}

defeatMusic :: IORef (Maybe Mix.Music)
defeatMusic = unsafePerformIO $ newIORef Nothing
{-# NOINLINE defeatMusic #-}

victoryMusic :: IORef (Maybe Mix.Music)
victoryMusic = unsafePerformIO $ newIORef Nothing
{-# NOINLINE victoryMusic #-}

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