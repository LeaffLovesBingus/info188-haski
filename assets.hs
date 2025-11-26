module Assets where

import Codec.Picture
import Codec.Picture.Types
import Graphics.Gloss
import Graphics.Gloss.Juicy
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromMaybe)
import qualified Data.Array as Array

import Types


-- Carga la spritesheet del jugador
playerSpriteSheet :: Maybe (Image PixelRGBA8)
playerSpriteSheet = unsafePerformIO $ do
    img <- readImage "assets/entidades/jugador.png"
    case img of
        Left _ -> return Nothing
        Right dyn -> return (Just (convertRGBA8 dyn))
{-# NOINLINE playerSpriteSheet #-}


-- Extraer un frame en específico
extractFrame :: Int -> Int -> Int -> Int -> Maybe (Image PixelRGBA8)
extractFrame col row w h = do
    sheet <- playerSpriteSheet
    let sx = col * w
        sy = row * h
    if sx + w > imageWidth sheet || sy + h > imageHeight sheet
        then Nothing
        else Just $ generateImage (\px py -> pixelAt sheet (sx + px) (sy + py)) w h


playerFrames :: Array.Array (Direction, AnimType, Int) Picture
playerFrames = unsafePerformIO $ do
    let w = 32
        h = 32

        -- filas del spritesheet
        rowIdleDown = 0
        rowIdleRight = 1
        rowIdleUp = 2
        rowWalkDown = 3
        rowWalkRight = 4
        rowWalkUp = 5

        extract col row =
            fromMaybe Blank (fromImageRGBA8 <$> extractFrame col row w h)
        
        idleFramesDown = [extract 0 rowIdleDown, extract 1 rowIdleDown]
        idleFramesRight = [extract 0 rowIdleRight, extract 1 rowIdleRight]
        idleFramesUp = [extract 0 rowIdleUp, extract 1 rowIdleUp]

        walkFramesDown = [extract i rowWalkDown  | i <- [0..3]]
        walkFramesRight = [extract i rowWalkRight | i <- [0..3]]
        walkFramesUp = [extract i rowWalkUp    | i <- [0..3]]

        -- izquierda = invertir derecha
        flipPic p = scale (-1) 1 p

        idleFramesLeft = map flipPic idleFramesRight
        walkFramesLeft = map flipPic walkFramesRight

        list =
            [ ((DirDown, Idle, i), idleFramesDown !! i) | i <- [0..1] ] ++
            [ ((DirRight, Idle, i), idleFramesRight !! i) | i <- [0..1] ] ++
            [ ((DirUp, Idle, i), idleFramesUp !! i) | i <- [0..1] ] ++
            [ ((DirLeft, Idle, i), idleFramesLeft !! i) | i <- [0..1] ] ++

            [ ((DirDown, Walk, i), walkFramesDown !! i) | i <- [0..3] ] ++
            [ ((DirRight, Walk, i), walkFramesRight !! i) | i <- [0..3] ] ++
            [ ((DirUp, Walk, i), walkFramesUp !! i) | i <- [0..3] ] ++
            [ ((DirLeft, Walk, i), walkFramesLeft !! i) | i <- [0..3] ]

    return (Array.array ((DirDown, Idle, 0), (DirLeft, Walk, 3)) list)
{-# NOINLINE playerFrames #-}