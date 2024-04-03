module Dibujos.Grilla
 where

import Dibujo (Dibujo, figura, juntar, apilar, rot45, rotar, encimar, espejar)
import FloatingPic(Conf(..), Output, half, zero)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss ( Picture, blue, red, color, pictures, rectangleSolid, scale, text, black, white, translate)


data Color = Azul | Rojo
    deriving (Show, Eq)

data BasicaSinColor = Rectangulo | Cruz | Triangulo | Efe
    deriving (Show, Eq)

type Basica = (BasicaSinColor, Color)

colorear :: Color -> Picture -> Picture
colorear Azul = color blue
colorear Rojo = color red

drawSquare :: Int -> Int -> Picture
drawSquare i j = pictures
    [ translate (fromIntegral $ j * 100) (fromIntegral $ (7-i) * 110) $ color white $ rectangleSolid 50 50
    , translate (fromIntegral $ j * 100) (fromIntegral $ (7-i) * 110) $ color black $ scale 0.2 0.2 $ text $ " (" ++ show i ++ "," ++ show j ++ ")"
    ]


drawGrid :: Output BasicaSinColor
drawGrid _ _ _ _ = pictures [drawSquare i j | i <- [0..7], j <- [0..7]]


interpBas :: Output Basica
interpBas (b, c) x y w = colorear c $ drawGrid b x y w



--NOTE: Hay que formar funciones que devuelvan Dibujo Basica para meter en la grilla.
figRoja :: BasicaSinColor -> Dibujo Basica
figRoja b = figura (b, Rojo)

row :: [Dibujo a] -> Dibujo a
row [] = error "row: no puede ser vacío"
row [d] = d
row (d:ds) = juntar 1 (fromIntegral $ length ds) d (row ds)

column :: [Dibujo a] -> Dibujo a
column [] = error "column: no puede ser vacío"
column [d] = d
column (d:ds) = apilar 1 (fromIntegral $ length ds) d (column ds)

grilla :: [[Dibujo a]] -> Dibujo a
grilla = column . map row

testAll :: Dibujo Basica
testAll = grilla [
     [figRoja Triangulo]
    ]


grillaConf :: Conf
grillaConf = Conf {
    name = "Grilla"
    , pic = testAll
    , bas = interpBas
}

-- NOTE: para compilar primero estar en la carpeta /paradigmas-24-lab-1-g45 y ejecutar --| cabal run dibujos Grilla |--