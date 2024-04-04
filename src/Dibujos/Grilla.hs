module Dibujos.Grilla
 where

import Dibujo (Dibujo, figura, juntar, apilar, encimar)
import FloatingPic(Conf(..), Output, half, zero)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss ( Picture, scale, text, translate)

data BasicaTuplas = Tupla (Int, Int, Float) deriving (Show, Eq)


-- Dibujamos la tupla.
drawTextTupla :: BasicaTuplas -> Picture
drawTextTupla (Tupla (x, y, z)) = scale z z $ text $ "(" ++ show x ++ "," ++ show y ++ ")"

-- Interpretamos la tupla para gloss con sus vectores.
interpBasicaTuplas :: Output BasicaTuplas
interpBasicaTuplas tupla (d_x, d_y) (w_x, _) (_, h_y) = translate (d_x + w_x/4) (d_y + h_y/2) $ drawTextTupla tupla

-- Generamos la figura a partir de una tupla.
figTupla :: BasicaTuplas -> Dibujo BasicaTuplas
figTupla b = figura b

-- Generamos una línea de la grilla.
drawLineGrilla :: Int -> Int -> Float-> [Dibujo BasicaTuplas]
drawLineGrilla x 0 z = [figTupla (Tupla (x, 0, z))]
drawLineGrilla x y z = drawLineGrilla x (y - 1) z ++ [figTupla (Tupla (x, y, z))]

-- Generamos la grilla x por y en un array de dibujos.
drawGrilla :: Int -> Int -> Float -> [[Dibujo BasicaTuplas]]
drawGrilla 0 y z = [drawLineGrilla 0 y z]
drawGrilla x y z = (drawGrilla (x-1) y z) ++ [(drawLineGrilla x y z)]

-- Generamos una fila a partir de un arreglo.
row :: [Dibujo a] -> Dibujo a
row [] = error "row: no puede ser vacío"
row [d] = d
row (d:ds) = juntar 1 (fromIntegral $ length ds) d (row ds)

-- Generamos una columna a partir de un arreglo.
column :: [Dibujo a] -> Dibujo a
column [] = error "column: no puede ser vacío"
column [d] = d
column (d:ds) = apilar 1 (fromIntegral $ length ds) d (column ds)

-- Generamos la grilla a partir de un arreglo.
grilla :: [[Dibujo a]] -> Dibujo a
grilla = column . map row

-- Generamos la grilla x por y.
grillaOut :: Int -> Int -> Float-> Dibujo BasicaTuplas
grillaOut x y z = grilla (drawGrilla x y z)

-- Exportamos la configuración de la grilla.
grillaConf :: Int -> Int -> Float-> Conf
grillaConf x y z = Conf {
     name = "Grilla"
    ,pic = grillaOut x y z 
    ,bas = interpBasicaTuplas
}

-- NOTE: Para compilar primero estar en la carpeta "/paradigmas-24-lab-1-g45" 
--       y ejecutar "cabal run dibujos Grilla"