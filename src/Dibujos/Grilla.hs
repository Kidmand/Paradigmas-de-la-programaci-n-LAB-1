module Dibujos.Grilla
 where

import Dibujo (Dibujo, figura, juntar, apilar, encimar)
import FloatingPic(Conf(..), Output, half, zero)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss ( Picture, scale, text, translate)

-- Resumen del tipo       ( x ,  y , fontSize) 
data BasicaTuplas = Tupla (Int, Int,   Float ) deriving (Show, Eq)


-- Dibujamos la tupla.
drawTextTupla :: BasicaTuplas -> Picture
drawTextTupla (Tupla (x, y, fontSize)) = scale fontSize fontSize $ text $ "(" ++ show x ++ "," ++ show y ++ ")"

-- Interpretamos la tupla para gloss con sus vectores.                         ---> FIXME: Nose porque con 4 queda bien, pero para 2 que es lo intuitivo no.
interpBasicaTuplas :: Output BasicaTuplas --                                   |
interpBasicaTuplas tupla (d_x, d_y) (w_x, _) (_, h_y) = translate (d_x + w_x/4) (d_y + h_y/2) $ drawTextTupla tupla

-- Generamos la figura a partir de una tupla.
figTupla :: BasicaTuplas -> Dibujo BasicaTuplas
figTupla b = figura b

-- Generamos una línea de la grilla.
drawLineGrilla :: Int -> Int -> Float-> [Dibujo BasicaTuplas]
drawLineGrilla x 0 fontSize = [figTupla (Tupla (x, 0, fontSize))]
drawLineGrilla x y fontSize = drawLineGrilla x (y - 1) fontSize ++ [figTupla (Tupla (x, y, fontSize))]

-- Generamos la grilla x por y en un array de dibujos.
drawGrilla :: Int -> Int -> Float -> [[Dibujo BasicaTuplas]]
drawGrilla 0 y fontSize = [drawLineGrilla 0 y fontSize]
drawGrilla x y fontSize = (drawGrilla (x-1) y fontSize) ++ [(drawLineGrilla x y fontSize)]

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
grillaOut x y fontSize = grilla (drawGrilla x y fontSize)

-- Exportamos la configuración de la grilla.
grillaConf :: Int -> Int -> Float-> Conf
grillaConf x y fontSize = Conf {
     name = "Grilla"
    ,pic = grillaOut x y fontSize 
    ,bas = interpBasicaTuplas
}

-- NOTE: Para compilar primero estar en la carpeta "/paradigmas-24-lab-1-g45" 
--       y ejecutar "cabal run dibujos Grilla"