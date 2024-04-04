module Dibujos.Grilla
 where

import Dibujo (Dibujo, figura, juntar, apilar, encimar)
import FloatingPic(Conf(..), Output, half, zero)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss ( Picture, scale, text, translate)

data BasicaTuplas = Tupla (Int, Int) deriving (Show, Eq)

s_val = 0.2 -- Función constante para el tamaño de la fuente.

-- Dibujamos la tupla.
drawTextTupla :: BasicaTuplas -> Picture
drawTextTupla (Tupla (x, y)) = scale s_val s_val $ text $ "(" ++ show x ++ "," ++ show y ++ ")"

-- Interpretamos la tupla para gloss con sus vectores.                         ---> FIXME: Nose porque con 4 queda bien, pero para 2 que es lo intuitivo no.
interpBasicaTuplas :: Output BasicaTuplas --                                   |
interpBasicaTuplas tupla (d_x, d_y) (w_x, w_y) (h_x, h_y) = translate (d_x + w_x/4) (d_y + h_y/2) $ drawTextTupla tupla

-- Generamos la figura a partir de una tupla.
figTupla :: BasicaTuplas -> Dibujo BasicaTuplas
figTupla b = figura b

-- Generamos una línea de la grilla.
drawLineGrilla :: Int -> Int -> [Dibujo BasicaTuplas]
drawLineGrilla x 0 = [figTupla (Tupla (x, 0))]
drawLineGrilla x y = drawLineGrilla x (y - 1) ++ [figTupla (Tupla (x, y))]

-- Generamos la grilla x por y en un array de dibujos.
drawGrilla :: Int -> Int -> [[Dibujo BasicaTuplas]]
drawGrilla 0 y = [drawLineGrilla 0 y]
drawGrilla x y = (drawGrilla (x-1) y) ++ [(drawLineGrilla x y)]

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
grillaOut :: Int -> Int -> Dibujo BasicaTuplas
grillaOut x y = grilla (drawGrilla x y)

-- Exportamos la configuración de la grilla.
grillaConf :: Int -> Int -> Conf
grillaConf x y = Conf {
     name = "Grilla"
    ,pic = grillaOut x y
    ,bas = interpBasicaTuplas
}

-- NOTE: Para compilar primero estar en la carpeta "/paradigmas-24-lab-1-g45" 
--       y ejecutar "cabal run dibujos Grilla"