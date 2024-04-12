module Pred (
  Pred,
  cambiar, anyDib, allDib, orP, andP, falla
) where

import Dibujo

-----------------------------------------------------------------------------------------------------------------------
-- NOTE: Definimos el tipo de dato Pred, que es una función que recibe un parámetro y devuelve un booleano.
type Pred a = a -> Bool
-----------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------------
-- Auxiliar de cambiar.
pf :: Pred a -> (a -> Dibujo a) -> a -> Dibujo a
pf pre f x
  | pre x = f x
  | otherwise = figura x

-- NOTE: Dado un predicado sobre básicas, cambiar todas las que satisfacen
--       el predicado por la figura básica indicada por el segundo argumento.
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

-----------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------------
-- Auxiliar de anyDib.
orIgnoreFloat :: Float -> Float -> Bool -> Bool -> Bool
orIgnoreFloat _ _ b1 b2 = b1 || b2

-- NOTE: Alguna básica satisface el predicado.
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
-----------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------------
-- Auxiliar de allDib.
andIgnoreFloat :: Float -> Float -> Bool -> Bool -> Bool
andIgnoreFloat _ _ b1 b2 = b1 && b2

-- NOTE: Todas las básicas satisfacen el predicado.
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
-----------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------------
-- Auxiliar de andP.
andP' :: Pred a -> Pred a -> a -> Bool
andP' p1 p2 x = p1 x && p2 x
-- NOTE: Los dos predicados se cumplen para el elemento recibido.
andP :: Pred a -> Pred a -> Pred a
andP p1 p2 = (andP' p1 p2) -- Currificación
-----------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------------
-- Auxiliar de orP.
orP' :: Pred a -> Pred a -> a -> Bool
orP' p1 p2 x = p1 x || p2 x
-- NOTE: Algún predicado se cumple para el elemento recibido.
orP :: Pred a -> Pred a -> Pred a
orP p1 p2 = (orP' p1 p2) -- Currificación
-----------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------------
-- NOTE: Falla siempre es True.
falla :: Bool
falla = True
-----------------------------------------------------------------------------------------------------------------------
