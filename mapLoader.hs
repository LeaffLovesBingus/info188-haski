module MapLoader (loadTMX) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (find)
import Data.Maybe (fromMaybe)

-- Carga el layer CSV (busca layer name="Fondo" si existe)
loadTMX :: FilePath -> IO [[Int]]
loadTMX path = do
    txt <- TIO.readFile path

    -- obtener width del <map ... width="NN"
    let width = readTextAttr "width" txt

    -- elegir layer (preferir "Fondo")
    let layers = T.splitOn "<layer" txt
        chosen = fromMaybe (head layers) (find (T.isInfixOf (T.pack "name=\"Fondo\"")) layers)

    -- extraer contenido CSV entre <data ...>...</data>
    let (_, afterData) = T.breakOn "<data" chosen
        afterGT = T.dropWhile (/='>') afterData
        csv = T.takeWhile (/='<') (T.drop 1 afterGT)

    -- parsear números
    let tokens = filter (not . T.null) $
                 map T.strip $ T.split (\c -> c == ',' || c == '\n' || c == '\r') csv
        ints = map (read . T.unpack) tokens

    return (chunksOf width ints)

-- helpers
readTextAttr :: T.Text -> T.Text -> Int
readTextAttr name txt =
    let pat = name <> T.pack "=\""
        (_, r) = T.breakOn pat txt
    in if T.null r
       then error $ "Attribute not found: " ++ T.unpack name
       else
         let after = T.drop (T.length pat) r
             val = T.takeWhile (/='"') after
         in read (T.unpack val)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)