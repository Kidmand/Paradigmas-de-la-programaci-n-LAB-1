module Dibujos.Escher where

import Dibujo (Dibujo, figura, juntar, apilar, rot45, rotar, encimar, espejar)
import FloatingPic(Conf(..), Output, half, zero)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss ( Picture, blue, red, color, pictures, rectangleSolid, scale, text, black, white, translate)


data Color = Azul deriving (Show, Eq)

data EscherTriangleSinColor = Triangulo   deriving (Show, Eq)

colorear :: Color -> Picture -> Picture
colorear Azul = color blue


type Escher = ( EscherTriangleSinColor, Color)

cuarteto :: Dibujo Escher -> Dibujo Escher -> Dibujo Escher -> Dibujo Escher  -> Dibujo Escher
cuarteto p q r s = apilar 1 1 (juntar 1 1 p q) (juntar 1 1 r s)

ciclo :: Dibujo Escher -> Dibujo Escher
ciclo p = cuarteto p (rotar p) (rotar(rotar p)) (rotar(rotar(rotar p)))

ov :: Picture -> Picture -> Picture
ov p q = pictures [p, q]
-- El dibujo u.
dibujoU :: Dibujo Escher -> Dibujo Escher
dibujoU p = undefined 

-- El dibujo t.
dibujoT :: Dibujo Escher -> Dibujo Escher
dibujoT p = undefined 

-- Esquina con nivel de detalle en base a la figura p.
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 1 f = cuarteto blank blank blank (dibujoU f)
esquina 2 f = cuarteto (esquina 1 f) (lado 1 f) rotar(lado 1 f) (dibujoU f)

-- Lado con nivel de detalle.
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 1 f = cuarteto blank blank (rotar (dibujoT f))  (dibujoT f)
lado 2 f = cuarteto (lado 1 f) (lado 1 f) (rotar f) f

-- Por suerte no tenemos que poner el tipo!
noneto p q r s t u v w x = undefined

-- El dibujo de Escher:
escher :: Int -> Escher -> Dibujo Escher
escher = undefined