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





--NOTE: Informacion importante
-----------------------------------------------------------------------------------------------------------------------

-- V.+ Es la operacion para la suma de vectores.
-- V.- Es la operacion para la resta de vectores.
-- V.* Es la operacion para la multiplicacion de vectores.
-- half(x V.k y) Multiplica el escalar 1/2 por el vector, en este caso k=(x V.P y): donde p = */+/- y k es un vector.
-- ((0,0) V.- x) Es la operacion para convertir el vector en negativo.

-- h = alto
-- w = ancho
-- d = origen

-----------------------------------------------------------------------------------------------------------------------





-- Interpretación de (^^^)
-----------------------------------------------------------------------------------------------------------------------
ov :: Picture -> Picture -> Picture
ov p q = pictures [p, q]

--Esta funcion es la encargada de superponer dos dibujos, uno arriba de otro. 
--Antes usabamos pictures [p,q] donde p y q son dos dibujos.
-----------------------------------------------------------------------------------------------------------------------



-- Rotar45 (b -> b)
-----------------------------------------------------------------------------------------------------------------------
r45 :: FloatingPic -> FloatingPic
--            f (       d+(w+h)/2         ,       (w+h)/2       ,         (h-w)/2    )
r45 f d w h = f (d V.+ (half (w V.+ h)))     (half (w V.+ h))         (half (h V.- w)) -- ---> f(d+(w+h)/2, (w+h)/2, (h-w)/2)                         
-----------------------------------------------------------------------------------------------------------------------



-- Rotar (b -> b)
-----------------------------------------------------------------------------------------------------------------------
rot :: FloatingPic -> FloatingPic 
--           f  (  d+w    ,  h  ,      -w     )
rot f d w h = f (d V.+ w)   (h)   ((0,0) V.- w) -- ---> f(d+w, h, -w)       
-----------------------------------------------------------------------------------------------------------------------



-- Espejar (b -> b)
-----------------------------------------------------------------------------------------------------------------------
esp :: FloatingPic -> FloatingPic
--            f(   d+w    ,      -w       ,  h)
esp f d w h = f (d V.+ w)   ((0,0) V.- w)   (h) -- ---> f(d+w, -w, h)
-----------------------------------------------------------------------------------------------------------------------



-- Superponer/Encimar (b -> b -> b) 
-----------------------------------------------------------------------------------------------------------------------
sup :: FloatingPic -> FloatingPic -> FloatingPic
--               f(d, w, h) ∪ g(d, w, h)
sup f g d w h = (f d w h) `ov` (g d w h)  -- ---> f(d, w, h) ∪ g(d, w, h)
-----------------------------------------------------------------------------------------------------------------------



-- Apilar (Float -> Float -> b -> b -> b)
-----------------------------------------------------------------------------------------------------------------------
api :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
--                            f (d + h'   , w ,   r*h   ) ∪ g(d, w, h') where r' = n/(m+n), r=m/(m+n), h'=r'*h
api m n f g d w h =   ((f (d V.+ h')  w  (r V.* h))) `ov` ((g d w h')) -- ---> f(d + h', w, r*h) ∪ g(d, w, h')
                   where r' = (n/(m+n))
                         r  = (m/(m+n))
                         h' = (r' V.* h)
-----------------------------------------------------------------------------------------------------------------------



-- Juntar  (Float -> Float -> b -> b -> b)
-----------------------------------------------------------------------------------------------------------------------
jun :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic                                                    

--                        FIXME: -------> Aca hay un error, x no esta declarada. Para mi va d.
--                              |
--                              |            
--                            f(x, w', h) ∪ g (   d+w'  ,    r'*w  , h) where r'=n/(m+n), r=m/(m+n), w'=r*w
jun m n f g d w h =  (f d w' h) `ov` (g (d V.+ w') (r' V.* w) h) -- ---> f(x, w', h) ∪ g(d+w', r'*w, h) 
                    where r  = (m/(m+n))
                          r' = (n/(m+n))
                          w' = (r V.* w)


--FIXME: Mejor explicacion del error: x aparece definida en las cosas que nos pasaron los profes,
--       pero si observamos bien la función, nos daremos cuenta que x no pinta nada, por lo que creo
--       que en realidad va d en lugar de x.

-----------------------------------------------------------------------------------------------------------------------



-----------------------------------------------------------------------------------------------------------------------

-- La función interp toma una función f que puede interpretar un valor de tipo a y 
-- produce una representación visual FloatingPic. 
-- Queremos modificar esta función f para que pueda interpretar valores del tipo Dibujo a, 
-- es decir, cambiar la interpretación para que funcione con dibujos. 
-- 
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