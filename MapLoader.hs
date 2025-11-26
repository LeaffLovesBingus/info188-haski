module MapLoader (loadMapFromJSON, chunksOf, TilesetInfo, TileLayer) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Vector as V
import Data.Scientific (toBoundedInteger)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import System.FilePath ((</>), takeDirectory, normalise)
import Control.Monad (forM)
import Control.Exception (try, IOException)
import Codec.Picture (readImage, convertRGBA8, DynamicImage)
import Codec.Picture.Types (Image, imageWidth)

type TilesetInfo = (Int, FilePath, Int, Int, Int) -- (firstgid, imagePath, tilewidth, tileheight, columns)
type TileLayer = (String, [[Int]])

-- Cargar mapa desde JSON (formato de Tiled)
-- Devuelve (tilesets, tileLayers, collisionMap)
loadMapFromJSON :: FilePath -> IO ([TilesetInfo], [TileLayer], [[Bool]])
loadMapFromJSON path = do
    bs <- B.readFile path
    case decode bs :: Maybe Value of
        Nothing -> error $ "No se pudo parsear JSON: " ++ path
        Just (Object root) -> do
            let baseDir = takeDirectory path

                tilesetsVal = KM.lookup (K.fromString "tilesets") root
                tilesetsArr = case tilesetsVal of
                                Just (Array a) -> V.toList a
                                _ -> []

                -- parse a tileset Value, handling external .tsx
                parseTilesetVal :: Value -> IO (Maybe TilesetInfo)
                parseTilesetVal (Object o) = do
                    let firstgid = case KM.lookup (K.fromString "firstgid") o of
                                    Just (Number n) -> fromMaybe (0 :: Int) (toBoundedInteger n :: Maybe Int)
                                    _ -> 0
                        -- try to get direct image/tile sizes from JSON tileset (may be absent when source is used)
                        mImage = case KM.lookup (K.fromString "image") o of
                                    Just (String t) -> Just (T.unpack t)
                                    _ -> Nothing
                        mTileW = case KM.lookup (K.fromString "tilewidth") o of
                                    Just (Number n) -> fromMaybe (0 :: Int) (toBoundedInteger n :: Maybe Int)
                                    _ -> 0
                        mTileH = case KM.lookup (K.fromString "tileheight") o of
                                    Just (Number n) -> fromMaybe (0 :: Int) (toBoundedInteger n :: Maybe Int)
                                    _ -> 0
                        mCols = case KM.lookup (K.fromString "columns") o of
                                    Just (Number n) -> fromMaybe (0 :: Int) (toBoundedInteger n :: Maybe Int)
                                    _ -> 0
                        mSource = case KM.lookup (K.fromString "source") o of
                                    Just (String t) -> Just (T.unpack t)
                                    _ -> Nothing
                    case (mImage, mSource) of
                        (Just img, _) -> do
                            -- image provided inline in JSON tileset
                            let imgPath = normalise (baseDir </> img)
                            let cols = if mCols > 0 then mCols else 0
                            return $ Just (firstgid, imgPath, mTileW, mTileH, cols)
                        (Nothing, Just src) -> do
                            let tsxPath = normalise (baseDir </> src)
                            mmeta <- parseTSX tsxPath
                            case mmeta of
                                Just (imgRel, tw, th, cols) -> do
                                    let imgPath = normalise (takeDirectory tsxPath </> imgRel)
                                    return $ Just (firstgid, imgPath, tw, th, cols)
                                Nothing -> return Nothing
                        _ -> return Nothing
                parseTilesetVal _ = return Nothing

            tilesets <- fmap catMaybes $ forM tilesetsArr $ \v -> parseTilesetVal v

            -- parse layers: collect all tilelayers in order
            let layersVal = KM.lookup (K.fromString "layers") root
                layersArr = case layersVal of
                              Just (Array a) -> V.toList a
                              _ -> []
                isTileLayer (Object o) = case KM.lookup (K.fromString "type") o of
                    Just (String t) -> t == T.pack "tilelayer"
                    _ -> False
                isTileLayer _ = False

                extractTileLayer (Object o) =
                    let name = case KM.lookup (K.fromString "name") o of
                                Just (String n) -> T.unpack n
                                _ -> ""
                        dataArr = case KM.lookup (K.fromString "data") o of
                                    Just (Array a) -> V.toList a
                                    _ -> []
                        ints = map (\v -> case v of
                                    Number n -> fromMaybe (0 :: Int) (toBoundedInteger n :: Maybe Int)
                                    _ -> 0) dataArr
                        width = case KM.lookup (K.fromString "width") o of
                                    Just (Number n) -> fromMaybe (0 :: Int) (toBoundedInteger n :: Maybe Int)
                                    _ -> 0
                        rows = if width <= 0 then [ints] else chunksOf width ints
                    in (name, rows)
                extractTileLayer _ = ("", [])

                tileLayers = [ extractTileLayer l | l@(Object _) <- layersArr, isTileLayer l ]

                -- buscar capa de colisiones por nombre (Colisiones, Collision, etc.)
                collisionNames = map T.pack ["Colisiones","Collision","Collisions","Colicion","Colision"]
                findCollision = listToMaybe [ l | l@(Object o) <- layersArr
                                                , let nm = case KM.lookup (K.fromString "name") o of
                                                              Just (String t) -> t
                                                              _ -> T.empty
                                                , nm `elem` collisionNames
                                                ]
                extractCollision (Object o) =
                    let dataArr = case KM.lookup (K.fromString "data") o of
                                    Just (Array a) -> V.toList a
                                    _ -> []
                        ints = map (\v -> case v of
                                    Number n -> fromMaybe (0 :: Int) (toBoundedInteger n :: Maybe Int)
                                    _ -> 0) dataArr
                        width = case KM.lookup (K.fromString "width") o of
                                    Just (Number n) -> fromMaybe (0 :: Int) (toBoundedInteger n :: Maybe Int)
                                    _ -> 0
                        rows = if width <= 0 then [ints] else chunksOf width ints
                    in map (map (/= (0 :: Int))) rows
                extractCollision _ = replicate (if null tileLayers then 0 else length (snd (head tileLayers))) (replicate (if null tileLayers then 0 else length (head (snd (head tileLayers)))) False)

                collisionMap = maybe (replicate (if null tileLayers then 0 else length (snd (head tileLayers))) (replicate (if null tileLayers then 0 else length (head (snd (head tileLayers)))) False)) extractCollision findCollision

            return (tilesets, tileLayers, collisionMap)

        _ -> error "JSON root no es un objeto"

-- Parsea un .tsx para extraer image source, tilewidth, tileheight y columnas (si falta columnas se calcula desde la imagen)
parseTSX :: FilePath -> IO (Maybe (FilePath, Int, Int, Int))
parseTSX tsxPath = do
    mtxt <- safeRead tsxPath
    case mtxt of
        Nothing -> return Nothing
        Just txt -> do
            let t = T.pack txt
                -- sección <tileset ...> ... </tileset>
                tilesetSec = let (_,r) = T.breakOn (T.pack "<tileset") t in if T.null r then T.empty else r
                -- sección <image .../>
                imageSec = let (_,r) = T.breakOn (T.pack "<image") tilesetSec in if T.null r then T.empty else r
                attrValue sec name =
                    let marker = T.pack (name ++ "=\"")
                        (_,rest) = T.breakOn marker sec
                    in if T.null rest then T.empty else T.takeWhile (/= '"') (T.drop (T.length marker) rest)
                twTxt = attrValue tilesetSec "tilewidth"
                thTxt = attrValue tilesetSec "tileheight"
                colsTxt = attrValue tilesetSec "columns"
                imgSrcTxt = attrValue imageSec "source"
                tileW = if T.null twTxt then 0 else read (T.unpack twTxt) :: Int
                tileH = if T.null thTxt then 0 else read (T.unpack thTxt) :: Int
                colsFromAttr = if T.null colsTxt then 0 else read (T.unpack colsTxt) :: Int
            if T.null imgSrcTxt || tileW <= 0 || tileH <= 0
               then return Nothing
               else do
                   let imgRel = T.unpack imgSrcTxt
                       imgFull = normalise (takeDirectory tsxPath </> imgRel)
                   -- si no hay columns, intentar calcular leyendo la imagen
                   cols <- if colsFromAttr > 0
                           then return colsFromAttr
                           else do
                               eimg <- try (readImage imgFull) :: IO (Either IOException (Either String DynamicImage))
                               case eimg of
                                   Right (Right dyn) -> do
                                       let img = convertRGBA8 dyn
                                       return $ (imageWidth img) `div` tileW
                                   _ -> return 0
                   return $ Just (imgRel, tileW, tileH, cols)

-- safeRead usando try
safeRead :: FilePath -> IO (Maybe String)
safeRead p = do
    r <- try (readFile p) :: IO (Either IOException String)
    case r of
        Right s -> return (Just s)
        Left _  -> return Nothing

-- helpers
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)