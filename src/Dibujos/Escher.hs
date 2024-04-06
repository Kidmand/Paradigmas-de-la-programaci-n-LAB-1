module Dibujos.Escher where


--NOTE: Importamos las librerías y módulos necesarios para construir nuestras funciones.
-----------------------------------------------------------------------------------------------------------------------
import Dibujo (Dibujo, figura, juntar, apilar, rot45, rotar, espejar, (^^^), cuarteto)
import FloatingPic(Conf(..), Output)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss ( Picture, white, color, magenta, line)
-----------------------------------------------------------------------------------------------------------------------


--NOTE: Definiciones de tipos y constructores de tipos.
-----------------------------------------------------------------------------------------------------------------------
data Color = Magenta | Blanco deriving (Show, Eq)

data EscherTriangleSinColor = Triangulo deriving (Show, Eq)

type Escher = (EscherTriangleSinColor, Color)
-----------------------------------------------------------------------------------------------------------------------



--NOTE: Estas funciones son las encargada de construirme/colorearme el triángulo y pasarle como
--      parámetro al interp.hs.
-----------------------------------------------------------------------------------------------------------------------

-- Define los colores en español según los contructores que le pasemos.
colorear :: Color -> Picture -> Picture
colorear Blanco = color white
colorear Magenta = color magenta

-- Construye el dibujo, que es de tipo Picture.
interpBasicaSinColor :: Output EscherTriangleSinColor
interpBasicaSinColor Triangulo x y w = line $ map (x V.+) [(0,0), y , w, (0,0)]

-- Colorea y pasa el dibujo como parámetro a la función escher, donde será interpretada por el interp.hs.
interpBas :: Output  Escher
interpBas (b, c) x y w = colorear c $ interpBasicaSinColor b x y w
-----------------------------------------------------------------------------------------------------------------------



--NOTE: Funciones que modularizan las funciones constructoras.
-----------------------------------------------------------------------------------------------------------------------

-- rotarN me rota n-veces la figura escher.
rotarN :: Int -> Dibujo Escher -> Dibujo Escher 
rotarN 0 f = f
rotarN n f = rotar (rotarN (n-1) f)

-- espejar_rot45 simplifica la sintáxis de lo que hace la función.
espejar_rot45 :: Dibujo Escher -> Dibujo Escher
espejar_rot45 k = espejar(rot45(k))

-- componer me superpone 4 dibujos escher y lo transforma en un sólo dibuo escher.
componer :: Dibujo Escher -> Dibujo Escher -> Dibujo Escher -> Dibujo Escher -> Dibujo Escher
componer x y w z = (x ^^^ y)  ^^^ (w ^^^ z)
-----------------------------------------------------------------------------------------------------------------------



--NOTE: Las funciones que siguen, son las constructoras del dibujo escher.
-----------------------------------------------------------------------------------------------------------------------

-- Artificio, en el artículo de hederson se usaba blank, pero en este caso blank es de la librería gloss y 
-- no tipa si lo ponemos como está en el artículo, por lo que simplemente blankEscher, devuelve un dibujo blanco.
blankEscher :: Dibujo Escher
blankEscher = figura (Triangulo, Blanco)


-- El dibujo u
dibujoU :: Dibujo Escher -> Dibujo Escher
dibujoU u1 =  componer u2 (rotar u2) (rotarN 2 u2) (rotarN 3 u2)
            where
            u2 = espejar_rot45 u1 

-- El dibujo t.
dibujoT :: Dibujo Escher -> Dibujo Escher
dibujoT t1 = (t1 ^^^ (t2 ^^^ t3)) 
            where
                t2 = espejar_rot45 t1
                t3 = rotarN 3 t2

-- Esquina con nivel de detalle en base a la figura p.
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 1 u = cuarteto blankEscher blankEscher blankEscher (dibujoU u)
esquina 2 u = cuarteto (esquina 1 u) (lado 1 u) (rotar(lado 1 u)) (dibujoU u)
esquina _ u = u

-- Lado con nivel de detalle.
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 1 t = cuarteto blankEscher blankEscher (rotar (dibujoT t))  (dibujoT t)
lado 2 t = cuarteto (lado 1 t) (lado 1 t) (rotar (dibujoT t)) (dibujoT t)
lado _ t = t
-----------------------------------------------------------------------------------------------------------------------



--NOTE: La función noneto básicamente es la que me construye el dibujo escher pasadole nueve dibujos.
--      igualmente esta es la definición de la función noneto.
-----------------------------------------------------------------------------------------------------------------------

-- Por suerte no tenemos que poner el tipo: pues se lo puse igual =).
noneto :: Dibujo Escher -> Dibujo Escher -> Dibujo Escher -> Dibujo Escher -> Dibujo Escher 
        -> Dibujo Escher -> Dibujo Escher -> Dibujo Escher -> Dibujo Escher -> Dibujo Escher
noneto p q r s t u v w x = apilar 1 2 (juntar 1 2 p (juntar 1 1 q r)) 
                        (apilar 1 1 (juntar 1 2 s (juntar 1 1 t u)) (juntar 1 2 v (juntar 1 1 w x)))
-----------------------------------------------------------------------------------------------------------------------



--NOTE: squarelimit2 es la encargada de pasar los nueve dibujos escher a noneto.
-----------------------------------------------------------------------------------------------------------------------
squarelimit2 :: Dibujo Escher -> Dibujo Escher
squarelimit2 u = noneto (esquina 2 u) (lado 2 u) (rotarN 3(esquina 2 u))
                        (rotar(lado 2 u))  (dibujoU u)  (rotarN 3 (lado 2 u))
                        (rotar(esquina 2 u)) (rotarN 2 (lado 2 u)) (rotarN 2 (esquina 2 u))                
-----------------------------------------------------------------------------------------------------------------------



--NOTE: Modularización de la función squarelimit2 para que no tengamos que pasar parámetros.
-----------------------------------------------------------------------------------------------------------------------
draw_Escher :: Dibujo Escher
draw_Escher = squarelimit2 (figura (Triangulo, Magenta))       
-----------------------------------------------------------------------------------------------------------------------          



--NOTE: Función encargada de pasar toda la información a interp.hs y main.hs.
-----------------------------------------------------------------------------------------------------------------------
escher :: Conf
escher = Conf {
    name = "Escher"
    , pic = draw_Escher
    , bas = interpBas
}
-----------------------------------------------------------------------------------------------------------------------


-- NOTE: Para compilar primero estar en la carpeta "/paradigmas-24-lab-1-g45" 
--       y ejecutar "cabal run dibujos Escher".
--       También pueden ejecutar "cabal run dibujos -- --lista" para ver todos 
--       los dibujos en una lista y elegir alguno para dibujar. 