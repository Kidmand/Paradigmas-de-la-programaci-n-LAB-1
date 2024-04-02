module Interp
  ( interp,
    initial,
  )
where

import Dibujo
import FloatingPic
import Graphics.Gloss (Display (InWindow), color, display, makeColorI, pictures, translate, white, Picture)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
initial :: Conf -> Float -> IO ()
--            -------------> nombre
--            |  ----------> dibujo                  ----> (withGrid fig size)
--            |  |    -----> interpretación básica   | 
--            |  |    |      -> Tamaño de la ventana |
--            |  |    |      |                       |
initial (Conf n dib intBas) size = display win white $ withGrid fig size
  where
    win = InWindow n (ceiling size, ceiling size) (0, 0) -- Crea una ventana de tamaño size x size con nombre n.
--                 ----> Vector -> Vector -> Vector -> Picture
--                / ---> Dibujo a -> FloatingPic
--               / /
--         (----/-/------) 
--        (----/--------------)    
    fig = ((interp intBas) dib) (0, 0) (size, 0) (0, size)  -- Obtiene la figura interpretada a mostrar. DEVUELVE UNA PICTURE.

    desp = -(size / 2) -- Calcula el desplazamiento para centrar la figura.
    withGrid p x = translate desp desp $ pictures [p, color grey $ grid (ceiling $ size / 10) (0, 0) x 10] -- Combina nuestra figura con la grilla.
    grey = makeColorI 100 100 100 100 -- Es el color gris que usamos para la grilla.

-- Interpretación de (^^^)
-- ?? Basica (a -> b) ?? 
ov :: Picture -> Picture -> Picture
ov p q = undefined

-- Rotar45 (b -> b)
f_r45 :: FloatingPic -- (Vector -> Vector -> Vector -> Picture)
-- FIXME: elegir una linea, la que ande o ver otra forma de hacer esto. 
f_r45 d w h = picture [ line [d V.+ (w V.+ h)/2], line [ (w  V.+ h)/2], line [(h  V.- w)/2]]
f_r45 d w h = line [d V.+ (w V.+ h)/2,(w  V.+ h)/2,(h  V.- w)/2]

r45 :: FloatingPic -> FloatingPic
r45 = f_r45 

-- Rotar (b -> b)
rot :: FloatingPic -> FloatingPic
rot = undefined

-- Espejar (b -> b)
esp :: FloatingPic -> FloatingPic
esp = undefined

-- Apilar (Float -> Float -> b -> b -> b)
api :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
api = undefined

-- Juntar  (Float -> Float -> b -> b -> b)
jun :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
jun = undefined

-- Encimar (b -> b -> b) 
sup :: FloatingPic -> FloatingPic -> FloatingPic
sup = undefined       

-- La función interp toma una función f que puede interpretar un valor de tipo a y 
-- produce una representación visual FloatingPic. 
-- Queremos modificar esta función f para que pueda interpretar valores del tipo Dibujo a, 
-- es decir, cambiar la interpretación para que funcione con dibujos. 
--                          ----- Vector -> Vector -> Vector -> Picture
--                          |
--                          |                            ---------------- Vector -> Vector -> Vector -> Picture
--           ----  a -> FloatingPic                      |
--           |          -------------  Dibujo a -> FloatingPic 
--           |          |
interp :: Output a -> Output (Dibujo a)
interp f = foldDib 
                      f 
                      r45
                      rot
                      esp
                      api 
                      jun 
                      sup
                      -- Dibujo a evaluar currificado. 
