module Pred (
  Pred,
  cambiar, anyDib, allDib, orP, andP, falla
) where

import Dibujo (Dibujo, figura, rot45, rotar, espejar, apilar, juntar, encimar, foldDib)

type Pred a = a -> Bool

-- -- Ejemplo de tipo de dato para probar las funciones.
-- data Figura = Cuadrado Int | Circulo Int | Triangulo Int Int Int
--   deriving (Show, Eq)

-- -- Predicado que indica si una figura es un cuadrado.
-- esCuadrado :: Pred Figura
-- esCuadrado (Cuadrado _) = True
-- esCuadrado _ = False
-- -- Ejemplo de uso: 
-- -- ghci> esCuadrado (Cuadrado 1)
-- -- True
-- -- ghci> esCuadrado (Circulo 1)
-- -- False

-- -- Ejemplo de dibujo para probar las funciones.
-- circulo n = figura (Circulo n)
-- cuadrado n = figura (Cuadrado n)
-- dibujo_de_ejemplo_1 = apilar 1 1 (circulo 1) (espejar (circulo 1))
-- dibujo_de_ejemplo_2 = juntar 1 1 (rot45 (cuadrado 1)) (rotar (cuadrado 1))
-- dibujo_de_ejemplo_3 = encimar (dibujo_de_ejemplo_2) (dibujo_de_ejemplo_1)
-- dibujo_de_ejemplo_4 = juntar 1 1 (rot45 (circulo 1)) (rotar (circulo 1))
-- dibujo_de_ejemplo_5 = encimar (dibujo_de_ejemplo_1) (dibujo_de_ejemplo_4)  


-- -- Funcion de transformacion. 
-- cuadradoACirculo :: Figura -> Dibujo Figura
-- cuadradoACirculo (Cuadrado n) = figura (Circulo (n*2))
-- cuadradoACirculo f = figura f

-- -- Compo probar: "cambiar esCuadrado cuadradoACirculo dibujo_de_ejemplo"

pf :: Pred a -> (a -> Dibujo a) -> a -> Dibujo a
pf pre f x
  | pre x = f x
  | otherwise = figura x

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar pre f d = foldDib 
                      (pf pre f) 
                      -- Cuando defines una función en Haskell con menos parámetros de los que necesita,
                      -- obtienes una nueva función como resultado, una función que espera los parámetros 
                      -- restantes. Esto es currificación.
                      rot45
                      rotar
                      espejar
                      apilar 
                      juntar 
                      encimar
                      d

orIgnoreFloat :: Float -> Float -> Bool -> Bool -> Bool
orIgnoreFloat _ _ b1 b2 = b1 || b2

-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib pre d = foldDib
                  pre
                  (False ||) -- Currificación
                  (False ||) -- Currificación
                  (False ||) -- Currificación
                  (orIgnoreFloat)
                  (orIgnoreFloat)
                  (||)
                  d

andIgnoreFloat :: Float -> Float -> Bool -> Bool -> Bool
andIgnoreFloat _ _ b1 b2 = b1 && b2

-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib pre d = foldDib
                  pre
                  (True &&) -- Currificación
                  (True &&) -- Currificación
                  (True &&) -- Currificación
                  (andIgnoreFloat)
                  (andIgnoreFloat)
                  (&&)
                  d

-- Auxiliar de andP.
andP' :: Pred a -> Pred a -> a -> Bool
andP' p1 p2 x = p1 x && p2 x
-- Los dos predicados se cumplen para el elemento recibido.
andP :: Pred a -> Pred a -> Pred a
andP p1 p2 = (andP' p1 p2) -- Currificación

-- Auxiliar de orP.
orP' :: Pred a -> Pred a -> a -> Bool
orP' p1 p2 x = p1 x || p2 x
-- Algún predicado se cumple para el elemento recibido.
orP :: Pred a -> Pred a -> Pred a
orP p1 p2 = (orP' p1 p2) -- Currificación

-- Falla siempre es True.
falla :: Bool
falla = True