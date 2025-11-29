module MapLoader (
    loadMapFromJSON,
    chunksOf,
    TilesetInfo,
    TileLayer,
    CollisionShape(..),
    loadGlobalCollisionShapesFromMap
) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Vector as V
import Data.Scientific (toBoundedInteger)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import System.FilePath ((</>), takeDirectory, normalise)
import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import Text.Read (readMaybe)
import Control.Exception (try, IOException)

-- Tipos
type TilesetInfo = (Int, String, Int, Int, Int)  -- (firstgid, imagePath, tileW, tileH, columns)
type TileLayer = (String, [[Int]])                -- (layerName, tileData)

-- Forma de colisión: rectángulo o polígono
data CollisionShape = CRect { cx :: Float, cy :: Float, cwidth :: Float, cheight :: Float }
                    | CPoly { cpoints :: [(Float, Float)] }
                    deriving (Show, Eq)

-- Dividir lista en chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Leer archivo de forma segura
safeReadFile :: FilePath -> IO (Maybe String)
safeReadFile path = do
    result <- try (readFile path) :: IO (Either IOException String)
    case result of
        Left _ -> return Nothing
        Right content -> return (Just content)

-- Cargar mapa desde JSON
loadMapFromJSON :: FilePath -> IO ([TilesetInfo], [TileLayer], [[Bool]])
loadMapFromJSON path = do
    content <- B.readFile path
    let mval = decode content :: Maybe Value
    case mval of
        Nothing -> return ([], [], [])
        Just val -> parseMap path val

-- Parsear el mapa
parseMap :: FilePath -> Value -> IO ([TilesetInfo], [TileLayer], [[Bool]])
parseMap jsonPath (Object obj) = do
    let width = fromMaybe 0 $ do
            Number n <- KM.lookup (K.fromString "width") obj
            toBoundedInteger n
        height = fromMaybe 0 $ do
            Number n <- KM.lookup (K.fromString "height") obj
            toBoundedInteger n

    -- Parsear tilesets
    tilesets <- case KM.lookup (K.fromString "tilesets") obj of
        Just (Array arr) -> parseTilesets jsonPath (V.toList arr)
        _ -> return []

    -- Parsear layers
    layers <- case KM.lookup (K.fromString "layers") obj of
        Just (Array arr) -> parseLayers width (V.toList arr)
        _ -> return []

    -- Crear mapa de colisiones vacío (se llenará con shapes)
    let emptyCollisions = replicate height (replicate width False)

    return (tilesets, layers, emptyCollisions)
parseMap _ _ = return ([], [], [])

-- Parsear tilesets
parseTilesets :: FilePath -> [Value] -> IO [TilesetInfo]
parseTilesets jsonPath vals = fmap catMaybes $ forM vals $ \v -> do
    case v of
        Object obj -> do
            let firstgid = fromMaybe 0 $ do
                    Number n <- KM.lookup (K.fromString "firstgid") obj
                    toBoundedInteger n
            -- Obtener source del TSX o imagen directa
            case KM.lookup (K.fromString "source") obj of
                Just (String srcTxt) -> do
                    let tsxPath = takeDirectory jsonPath </> T.unpack srcTxt
                    parseTSX firstgid tsxPath
                _ -> return Nothing
        _ -> return Nothing

-- Parsear archivo TSX
parseTSX :: Int -> FilePath -> IO (Maybe TilesetInfo)
parseTSX firstgid tsxPath = do
    content <- safeReadFile tsxPath
    case content of
        Nothing -> return Nothing
        Just txt -> do
            let t = T.pack txt
                -- Extraer atributos del tileset
                tileW = fromMaybe 32 $ extractAttr t "tilewidth"
                tileH = fromMaybe 32 $ extractAttr t "tileheight"
                cols = fromMaybe 16 $ extractAttr t "columns"
                -- Extraer ruta de imagen
                imgSrc = extractImageSource t
                imgPath = case imgSrc of
                    Just src -> takeDirectory tsxPath </> T.unpack src
                    Nothing -> ""
            return $ Just (firstgid, normalise imgPath, tileW, tileH, cols)

-- Extraer atributo numérico de XML
extractAttr :: T.Text -> String -> Maybe Int
extractAttr txt attrName =
    let marker = T.pack (attrName ++ "=\"")
        (_, rest) = T.breakOn marker txt
    in if T.null rest
       then Nothing
       else readMaybe . T.unpack $ T.takeWhile (/= '"') (T.drop (T.length marker) rest)

-- Extraer source de imagen
extractImageSource :: T.Text -> Maybe T.Text
extractImageSource txt =
    let marker = T.pack "source=\""
        (_, rest) = T.breakOn marker txt
    in if T.null rest
       then Nothing
       else Just $ T.takeWhile (/= '"') (T.drop (T.length marker) rest)

-- Parsear layers
parseLayers :: Int -> [Value] -> IO [TileLayer]
parseLayers width vals = fmap catMaybes $ forM vals $ \v -> do
    case v of
        Object obj -> do
            let mName = case KM.lookup (K.fromString "name") obj of
                    Just (String s) -> Just (T.unpack s)
                    _ -> Nothing
                mData = case KM.lookup (K.fromString "data") obj of
                    Just (Array arr) -> Just $ map extractInt (V.toList arr)
                    _ -> Nothing
            case (mName, mData) of
                (Just name, Just tileData) ->
                    return $ Just (name, chunksOf width tileData)
                _ -> return Nothing
        _ -> return Nothing

extractInt :: Value -> Int
extractInt (Number n) = fromMaybe 0 (toBoundedInteger n)
extractInt _ = 0

--------------------------------------------------------------------------------
-- COLISIONES: Cargar shapes de colisión desde los TSX
--------------------------------------------------------------------------------

-- Cargar todas las shapes de colisión del mapa (GID global -> [CollisionShape])
loadGlobalCollisionShapesFromMap :: FilePath -> IO (Map.Map Int [CollisionShape])
loadGlobalCollisionShapesFromMap jsonPath = do
    content <- B.readFile jsonPath
    let mval = decode content :: Maybe Value
    case mval of
        Nothing -> return Map.empty
        Just (Object obj) -> do
            case KM.lookup (K.fromString "tilesets") obj of
                Just (Array arr) -> do
                    results <- forM (V.toList arr) $ \v -> do
                        case v of
                            Object tsObj -> do
                                let firstgid = fromMaybe 0 $ do
                                        Number n <- KM.lookup (K.fromString "firstgid") tsObj
                                        toBoundedInteger n
                                case KM.lookup (K.fromString "source") tsObj of
                                    Just (String srcTxt) -> do
                                        let tsxPath = takeDirectory jsonPath </> T.unpack srcTxt
                                        localShapes <- parseTSXCollisionShapes tsxPath
                                        -- Debug: mostrar qué se cargó
                                        let globalShapes = Map.mapKeys (+ firstgid) localShapes
                                        if Map.size localShapes > 0
                                            then putStrLn $ "  TSX " ++ T.unpack srcTxt ++ " (firstgid=" ++ show firstgid ++ "): " ++ show (Map.size localShapes) ++ " tiles con colisión"
                                            else return ()
                                        return globalShapes
                                    _ -> return Map.empty
                            _ -> return Map.empty
                    return $ Map.unions results
                _ -> return Map.empty
        _ -> return Map.empty

-- Parsear shapes de colisión de un TSX (tileId local -> [CollisionShape])
parseTSXCollisionShapes :: FilePath -> IO (Map.Map Int [CollisionShape])
parseTSXCollisionShapes tsxPath = do
    content <- safeReadFile tsxPath
    case content of
        Nothing -> return Map.empty
        Just txt -> do
            let t = T.pack txt
                -- Buscar todos los bloques <tile id="N">...</tile>
                tileBlocks = extractTileBlocks t
                -- Para cada bloque, extraer shapes
                results = mapMaybe parseTileBlock tileBlocks
            return $ Map.fromList results

-- Extraer bloques de tile del XML
extractTileBlocks :: T.Text -> [T.Text]
extractTileBlocks txt =
    let parts = T.splitOn (T.pack "<tile id=\"") txt
    in drop 1 parts  -- Ignorar lo anterior al primer <tile

-- Parsear un bloque de tile y extraer su ID y shapes
parseTileBlock :: T.Text -> Maybe (Int, [CollisionShape])
parseTileBlock block = do
    -- Extraer el ID del tile
    let idTxt = T.takeWhile (/= '"') block
    tileId <- readMaybe (T.unpack idTxt) :: Maybe Int
    -- Extraer shapes del objectgroup
    let shapes = extractShapesFromBlock block
    -- Solo devolver si hay shapes válidas (con área > 0)
    let validShapes = filter hasArea shapes
    if null validShapes
        then Nothing
        else Just (tileId, validShapes)

-- Verificar si un shape tiene área
hasArea :: CollisionShape -> Bool
hasArea (CRect _ _ w h) = w > 0.1 && h > 0.1
hasArea (CPoly pts) = length pts >= 3

-- Extraer shapes de un bloque de tile
extractShapesFromBlock :: T.Text -> [CollisionShape]
extractShapesFromBlock block =
    -- Buscar <objectgroup> y extraer objetos
    let objParts = T.splitOn (T.pack "<object ") block
    in mapMaybe parseObject (drop 1 objParts)

-- Parsear un objeto individual a CollisionShape
parseObject :: T.Text -> Maybe CollisionShape
parseObject objTxt =
    let getAttr name =
            let marker = T.pack (name ++ "=\"")
                (_, rest) = T.breakOn marker objTxt
            in if T.null rest
               then Nothing
               else Just $ T.takeWhile (/= '"') (T.drop (T.length marker) rest)
        
        mx = getAttr "x" >>= readMaybe . T.unpack :: Maybe Float
        my = getAttr "y" >>= readMaybe . T.unpack :: Maybe Float
        mw = getAttr "width" >>= readMaybe . T.unpack :: Maybe Float
        mh = getAttr "height" >>= readMaybe . T.unpack :: Maybe Float
        mPoints = getAttr "points"
        mRotation = getAttr "rotation" >>= readMaybe . T.unpack :: Maybe Float
        
        -- Offset del objeto (default 0,0)
        ox = fromMaybe 0 mx
        oy = fromMaybe 0 my
        -- Rotación en grados (convertir a radianes)
        rotDeg = fromMaybe 0 mRotation
        rotRad = rotDeg * pi / 180.0
        
        -- Rotar un punto alrededor del origen (0,0) del objeto
        rotatePoint :: (Float, Float) -> (Float, Float)
        rotatePoint (px, py) =
            let cosR = cos rotRad
                sinR = sin rotRad
            in (px * cosR - py * sinR, px * sinR + py * cosR)
        
    in case mPoints of
        -- Es un polígono - los puntos son relativos al (x,y) del objeto
        Just ptsTxt ->
            let pairs = T.splitOn (T.pack " ") ptsTxt
                parsePair p = case T.splitOn (T.pack ",") p of
                    [a, b] -> case (readMaybe (T.unpack a), readMaybe (T.unpack b)) of
                        (Just px, Just py) -> 
                            -- Rotar el punto y luego sumar el offset
                            let (rx, ry) = rotatePoint (px, py)
                            in Just (ox + rx, oy + ry)
                        _ -> Nothing
                    _ -> Nothing
                pts = mapMaybe parsePair pairs
            in if length pts >= 3
               then Just (CPoly pts)
               else Nothing
        -- Es un rectángulo
        Nothing ->
            case (mx, my, mw, mh) of
                (Just x, Just y, Just w, Just h) ->
                    if w > 0.1 && h > 0.1
                    then Just (CRect x y w h)
                    else Nothing
                _ -> Nothing