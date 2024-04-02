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
-- dibujo_de_ejemplo = apilar 1 1 (figura (Cuadrado 1)) (figura (Circulo 1))

-- -- Funcion de transformacion. 
-- cuadradoACirculo :: Figura -> Dibujo Figura
-- cuadradoACirculo (Cuadrado n) = figura (Circulo (n*2))
-- cuadradoACirculo f = figura f

-- Compo probar: "cambiar esCuadrado cuadradoACirculo dibujo_de_ejemplo"

pf :: Pred a -> (a -> Dibujo a) -> a -> Dibujo a
pf pred f x
  | pred x = f x
  | otherwise = figura x

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar pred f d = foldDib 
                      (pf pred f) 
                      -- Cuando defines una función en Haskell con menos parámetros de los que necesita,
                      -- obtienes una nueva función como resultado, una función que espera los parámetros 
                      -- restantes. Esto se llama currying.
                      rot45
                      rotar
                      espejar
                      apilar 
                      juntar 
                      encimar
                      d

-- Alguna básica satisface el predicado.
anyDib = undefined

-- Todas las básicas satisfacen el predicado.
allDib = undefined

-- Los dos predicados se cumplen para el elemento recibido.
andP = undefined

-- Algún predicado se cumple para el elemento recibido.
orP = undefined

falla = True