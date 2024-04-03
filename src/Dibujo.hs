module Dibujo (
    figura,
    encimar,
    apilar,
    juntar,
    rot45,
    rotar,
    espejar,
    (^^^),
    (.-.),
    (///),
    r90,
    r180,
    r270,
    encimar4,
    cuarteto,
    ciclar,
    mapDib,
    change,
    foldDib,
    figuras,
    Dibujo,
    ) where


-- nuestro lenguaje 
data Dibujo a = Basica a
              | Encimar (Dibujo a) (Dibujo a)
              | Apilar Float Float (Dibujo a) (Dibujo a)
              | Juntar Float Float (Dibujo a) (Dibujo a)
              | Rot45 (Dibujo a)
              | Rotar (Dibujo a)
              | Espejar (Dibujo a)
              deriving (Eq,Show)

-- combinadores
infixr 6 ^^^ -- Superpone un dibujo con otro.

infixr 7 .-. -- Pone el primer dibujo arriba del segundo, ambos ocupan el mismo espacio.

infixr 8 /// -- Pone un dibujo al lado del otro, ambos ocupan el mismo espacio.

-- Compone una función n veces a un elemento.
comp :: Int -> (a -> a) -> a -> a
comp n f x
  | n < 0     = error "No se puede componer negativamente"
  | n == 0    = x
  | otherwise = f (comp (n-1) f x)

-- Funciones constructoras
figura :: a -> Dibujo a
figura d = Basica d

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar d_1 d_2 = Encimar d_1 d_2

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar x y d_1 d_2 = Apilar x y d_1 d_2

juntar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar x y d_1 d_2 = Juntar x y d_1 d_2

rot45 :: Dibujo a -> Dibujo a
rot45 d = Rot45 d

rotar :: Dibujo a -> Dibujo a
rotar d = Rotar d

espejar :: Dibujo a -> Dibujo a
espejar d = Espejar d

-- Superpone dos dibujos uno arriba del otro.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) d_1 d_2 = encimar d_1 d_2

-- Pone un dibujo arriba del otro, ambos ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) d_1 d_2 = apilar 1 1 d_1 d_2

-- Pone un dibujo al lado del otro, ambos ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) d_1 d_2 = juntar 1 1 d_1 d_2

-- rotaciones
r90 :: Dibujo a -> Dibujo a
r90 d = comp 2 rot45 d

r180 :: Dibujo a -> Dibujo a
r180 d = comp 2 r90 d

r270 :: Dibujo a -> Dibujo a
r270 d = comp 3 r90 d

-- una figura repetida con las cuatro rotaciones, superimpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 d = (d) ^^^ (r90 d) ^^^ (r180 d) ^^^ (r270 d)

-- cuatro figuras en un cuadrante.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto d_0 d_1 d_2 d_3 = (d_0 /// d_1) .-. (d_2 /// d_3)

-- un cuarteto donde se repite la imagen, rotada (¡No confundir con encimar4!)
ciclar :: Dibujo a -> Dibujo a
ciclar d = cuarteto d (r90 d) (r180 d) (r270 d)

-- map para nuestro lenguaje
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Basica d) = Basica (f d)
mapDib f (Encimar d_1 d_2) = Encimar (mapDib f d_1) (mapDib f d_2)
mapDib f (Apilar x y d_1 d_2) = Apilar x y (mapDib f d_1) (mapDib f d_2)
mapDib f (Juntar x y d_1 d_2) = Juntar x y (mapDib f d_1) (mapDib f d_2)
mapDib f (Rot45 d) = Rot45 (mapDib f d)
mapDib f (Rotar d) = Rotar (mapDib f d)
mapDib f (Espejar d) = Espejar (mapDib f d)
-- verificar que las operaciones satisfagan
-- 1. map figura = id
-- 2. map (g . f) = mapDib g . mapDib f

-- Cambiar todas las básicas de acuerdo a la función. 
change :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
change f (Basica d) = f d
change f (Encimar d_1 d_2) = Encimar (change f d_1) (change f d_2)
change f (Apilar x y d_1 d_2) = Apilar x y (change f d_1) (change f d_2)
change f (Juntar x y d_1 d_2) = Juntar x y (change f d_1) (change f d_2)
change f (Rot45 d) = Rot45 (change f d)
change f (Rotar d) = Rotar (change f d)
change f (Espejar d) = Espejar (change f d)


-- Principio de recursión para Dibujos.
foldDib ::
  (a -> b) ->                        -- Funcion para Basicas -> f1
  (b -> b) ->                        -- Funcion para Rotar45 -> f2
  (b -> b) ->                        -- Funcion para Rotar   -> f3
  (b -> b) ->                        -- Funcion para Espejar -> f4
  (Float -> Float -> b -> b -> b) -> -- Funcion para Apilar  -> f5
  (Float -> Float -> b -> b -> b) -> -- Funcion para Juntar  -> f6
  (b -> b -> b) ->                   -- Funcion para Encimar -> f7
  Dibujo a                           -- Dibujo a evaluar.
  -> b                               -- Resultado.
foldDib f1 _ _ _ _ _ _ (Basica d) = f1 d
foldDib f1 f2 f3 f4 f5 f6 f7 (Rot45 d) = f2 (foldDib f1 f2 f3 f4 f5 f6 f7 d)
foldDib f1 f2 f3 f4 f5 f6 f7 (Rotar d) = f3 (foldDib f1 f2 f3 f4 f5 f6 f7 d)
foldDib f1 f2 f3 f4 f5 f6 f7 (Espejar d) = f4 (foldDib f1 f2 f3 f4 f5 f6 f7 d)
foldDib f1 f2 f3 f4 f5 f6 f7 (Apilar x y d_1 d_2) = f5 x y (foldDib f1 f2 f3 f4 f5 f6 f7 d_1) (foldDib f1 f2 f3 f4 f5 f6 f7 d_2)
foldDib f1 f2 f3 f4 f5 f6 f7 (Juntar x y d_1 d_2) = f6 x y (foldDib f1 f2 f3 f4 f5 f6 f7 d_1) (foldDib f1 f2 f3 f4 f5 f6 f7 d_2)
foldDib f1 f2 f3 f4 f5 f6 f7 (Encimar d_1 d_2) = f7 (foldDib f1 f2 f3 f4 f5 f6 f7 d_1) (foldDib f1 f2 f3 f4 f5 f6 f7 d_2) 

-- Extrae todas las figuras básicas de un dibujo.
figuras :: Dibujo a -> [a]
figuras (Basica d) = [d]
figuras (Encimar d_1 d_2) = figuras d_1 ++ figuras d_2
figuras (Apilar _ _ d_1 d_2) = figuras d_1 ++ figuras d_2
figuras (Juntar _ _ d_1 d_2) = figuras d_1 ++ figuras d_2
figuras (Rot45 d) = figuras d
figuras (Rotar d) = figuras d
figuras (Espejar d) = figuras d


