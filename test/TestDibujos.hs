module Main (main) where

import Dibujo
import qualified System.Exit as Exit
import Test.HUnit

-- Prueba para la función figura.
testFigura :: Test
testFigura =
  TestCase $
    assertEqual
      "La función figura debería crear un dibujo básico"
      (Basica "círculo")
      (figura "círculo")

-- Prueba para la función encimar.
testEncimar :: Test
testEncimar =
  TestCase $
    assertEqual
      "La función encimar debería encimar dos dibujos"
      (Encimar (Basica "rectangulo") (Basica "rectangulo"))
      (encimar (Basica "rectangulo") (Basica "rectangulo"))

-- Prueba para la función apilar.
testApilar :: Test
testApilar =
  TestCase $
    assertEqual
      "La función apilar debería apilar dos dibujos"
      (Apilar 3 4 (Basica "rectangulo") (Basica "rectangulo"))
      (apilar 3 4 (Basica "rectangulo") (Basica "rectangulo"))

-- Prueba para la función rot45.
testRot45 :: Test
testRot45 =
  TestCase $
    assertEqual
      "La función rot45 debería rotarme el dibujo a 45º"
      (Rot45 (Basica "rectangulo"))
      (rot45 (Basica "rectangulo"))

-- Prueba para la función rotar.
testRotar :: Test
testRotar =
  TestCase $
    assertEqual
      "La función rotar debería rotarme el dibujo sobre el eje central"
      (Rotar (Basica "rectangulo"))
      (rotar (Basica "rectangulo"))

-- Prueba para la función espejar.
testEspejar :: Test
testEspejar =
  TestCase $
    assertEqual
      "La función espejar debería espejarme el dibujo"
      (Espejar (Basica "rectangulo"))
      (espejar (Basica "rectangulo"))

-- Prueba para la función (^^^).
testSuperponer :: Test
testSuperponer =
  TestCase $
    assertEqual
      "La función (^^^)  debería superponer dos dibujos"
      (Encimar (Basica "rectangulo") (Basica "rectangulo"))
      ((^^^) (Basica "rectangulo") (Basica "rectangulo"))

-- Prueba para la función (.-.).
testApilar_2 :: Test
testApilar_2 =
  TestCase $
    assertEqual
      "La función (.-.)  debería apilar dos dibujos"
      (Apilar 1 1 (Basica "rectangulo") (Basica "rectangulo"))
      ((.-.) (Basica "rectangulo") (Basica "rectangulo"))

-- Prueba para la función (///).
testJuntar :: Test
testJuntar =
  TestCase $
    assertEqual
      "La función (///)  debería juntar dos dibujos"
      (Juntar 1 1 (Basica "rectangulo") (Basica "rectangulo"))
      ((///) (Basica "rectangulo") (Basica "rectangulo"))

-- Prueba para la función r90.
testR90 :: Test
testR90 =
  TestCase $
    assertEqual
      "La función r90  debería componer dos veces rot45 en el dibujo"
      (Rot45 (Rot45 (Basica "rectangulo")))
      (r90 (Basica "rectangulo"))

-- Prueba para la función r180.
testR180 :: Test
testR180 =
  TestCase $
    assertEqual
      "La función r180  debería componer dos veces rot90 en el dibujo"
      (Rot45 (Rot45 (Rot45 (Rot45 (Basica "rectangulo")))))
      (r180 (Basica "rectangulo"))

-- Prueba para la función r270.
testR270 :: Test
testR270 =
  TestCase $
    assertEqual
      "La función r270  debería componer tres veces rot90 en el dibujo"
      (Rot45 (Rot45 (Rot45 (Rot45 (Rot45 (Rot45 (Basica "rectangulo")))))))
      (r270 (Basica "rectangulo"))

rot_2_45 :: Dibujo String
rot_2_45 = Rot45 (Rot45 (Basica "rectangulo"))

rot_4_45 :: Dibujo String
rot_4_45 = Rot45 (Rot45 (Rot45 (Rot45 (Basica "rectangulo"))))

rot_6_45 :: Dibujo String
rot_6_45 = Rot45 (Rot45 (Rot45 (Rot45 (Rot45 (Rot45 (Basica "rectangulo"))))))

-- Prueba para la función encimar4.
testEncimar4 :: Test
testEncimar4 =
  TestCase $
    assertEqual
      "La función encimar4 debería encimar el dibujo 4 veces rotando 90º,180º,270º"
      (Encimar (Basica "rectangulo") (Encimar rot_2_45 (Encimar rot_4_45 rot_6_45)))
      (encimar4 (Basica "rectangulo"))

-- Prueba para la función cuarteto.
testCuarteto :: Test
testCuarteto =
  TestCase $
    assertEqual
      "La función cuarteto deberia hacer cuatro dibujos en un cuadrante."
      ( Apilar
          1
          1
          (Juntar 1 1 (Basica "rectangulo") (Basica "rectangulo"))
          (Juntar 1 1 (Basica "rectangulo") (Basica "rectangulo"))
      )
      (cuarteto (Basica "rectangulo") (Basica "rectangulo") (Basica "rectangulo") (Basica "rectangulo"))

-- Prueba para la función ciclar.
testCiclar :: Test
testCiclar =
  TestCase $
    assertEqual
      "La función ciclar debería hacer un cuarteto donde se repite la imagen"
      ( Apilar
          1
          1
          (Juntar 1 1 (Basica "rectangulo") rot_2_45)
          (Juntar 1 1 rot_4_45 rot_6_45)
      )
      (ciclar (Basica "rectangulo"))

-- Prueba para la función juntar.
-- La función (juntar x y d1 d2) debería combinar los dibujos d1 y d2 separados por x ancho e y alto.
test_juntar :: Test
test_juntar =
  TestCase $
    assertEqual
      "Deberia juntar dos dibujos separados por x=3 ancho e y=4 alto."
      -- Valor esperado
      (Juntar 3 4 (Basica "rectangulo") (Basica "rectangulo"))
      -- Valor obtenido
      (juntar 3 4 (Basica "rectangulo") (Basica "rectangulo"))

-- Prueba para la función mapDib.
-- La función (mapDib f d) debería aplicar la funcion f a cada Basica del dibujo d.
test_mapDib :: Test
test_mapDib =
  TestCase $
    assertEqual
      "Deberia sumar 1 a cada básica del dibujo."
      -- Valor esperado
      (Juntar 1 1 (Basica 2) (Basica 2))
      -- Valor obtenido
      (mapDib f (Juntar 1 1 (Basica 1) (Basica 1)))
  where
    f x = x + 1

-- verificar que las operaciones satisfagan
-- 1. map figura = id
-- 2. map (g . f) = mapDib g . mapDib f

-- Prueba para la función change.
-- La función (change f d) debería cambiar cada básica del dibujo d con el resultado de aplicar la función f.
test_change :: Test
test_change =
  TestCase $
    assertEqual
      "Deberia cambiar cada básica del dibujo por un circulo."
      -- Valor esperado
      (Juntar 1 1 (Basica "circulo") (Rot45 (Basica "circulo")))
      -- Valor obtenido
      (change f (Juntar 1 1 (Basica "rectangulo") (Rot45 (Basica "rectangulo"))))
  where
    f :: String -> Dibujo String
    f b = Basica "circulo" -- Cambia cualquier básica por un círculo

-- Prueba para la función foldDib.
-- La función (foldDib f1 f2 f3 f4 f5 f6 f7 d) debería aplicar en el dibujo d
-- la función f1 a las Basicas, f2 a Rot45, f3 a Rotar, f4 a Espejar, f5 a Apilar, f6 a Juntar, f7 a Encimar.
test_foldDib :: Test
test_foldDib =
  TestCase $
    assertEqual
      "Deberia cambiar cada básica del dibujo por un circulo. Y cada funcion por otra."
      -- Valor esperado
      (Apilar 1 1 (Rotar (Rot45 (Basica "circulo"))) (Juntar 2.0 2.0 (Basica "circulo") (Encimar (Basica "circulo") (Basica "circulo"))))
      -- Valor obtenido
      (foldDib f1 f2 f3 f4 f5 f6 f7 (Juntar 1 1 (Rot45 (Espejar (Basica "rectangulo"))) (Apilar 2 2 (Basica "rectangulo") (Encimar (Basica "rectangulo") (Basica "rectangulo")))))
  where
    f1 b = Basica "circulo" -- Función para Basica
    f2 = Rotar -- Función para Rot45
    f3 = Espejar -- Función para Rotar
    f4 = Rot45 -- Función para Espejar
    f5 = Juntar -- Función para Apilar
    f6 = Apilar -- Función para Juntar
    f7 = Encimar -- Función para Encimar

-- Prueba para la función foldDib.
-- La función (foldDib f1 f2 f3 f4 f5 f6 f7 d) debería aplicar en el dibujo d
-- la función f1 a las Basicas, f2 a Rot45, f3 a Rotar, f4 a Espejar, f5 a Apilar, f6 a Juntar, f7 a Encimar.
test_foldDib_2 :: Test
test_foldDib_2 =
  TestCase $
    assertEqual
      "Deberia cambiar cada básica del dibujo por un circulo."
      -- Valor esperado
      (Juntar 1 1 (Rot45 (Espejar (Basica "circulo"))) (Apilar 2 2 (Basica "circulo") (Encimar (Basica "circulo") (Basica "circulo"))))
      -- Valor obtenido
      (foldDib f1 f2 f3 f4 f5 f6 f7 (Juntar 1 1 (Rot45 (Espejar (Basica "rectangulo"))) (Apilar 2 2 (Basica "rectangulo") (Encimar (Basica "rectangulo") (Basica "rectangulo")))))
  where
    f1 b = Basica "circulo" -- Función para Basica
    f2 = Rot45 -- Función para Rot45
    f3 = Rotar -- Función para Rotar
    f4 = Espejar -- Función para Espejar
    f5 = Apilar -- Función para Apilar
    f6 = Juntar -- Función para Juntar
    f7 = Encimar -- Función para Encimar

-- Prueba para la función figuras.
-- La función (figuras d) debería devolver un array con cada básica del dibujo b.
test_figuras :: Test
test_figuras =
  TestCase $
    assertEqual
      "Deberia devolver un array con cada básica del dibujo."
      -- Valor esperado
      ["rectangulo", "triangulo", "rectangulo", "triangulo"]
      -- Valor obtenido
      (figuras (apilar 1 1 d b))
  where
    d = juntar 1 5 (rot45 (Basica "rectangulo")) (rot45 (Basica "triangulo"))
    b = juntar 2 3 (Basica "rectangulo") (Basica "triangulo")

-- Lista de todas las pruebas
tests :: Test
tests =
  TestList
    [ TestLabel "test_figura" testFigura,
      TestLabel "test_encimar" testEncimar,
      TestLabel "test_apilar" testApilar,
      TestLabel "test_juntar" test_juntar,
      TestLabel "test_rot45" testRot45,
      TestLabel "test_rotar" testRotar,
      TestLabel "test_espejar" testEspejar,
      TestLabel "test_^^^" testSuperponer,
      TestLabel "test_.-." testApilar_2,
      TestLabel "test_///" testJuntar,
      TestLabel "test_r90" testR90,
      TestLabel "test_r180" testR180,
      TestLabel "test_r270" testR270,
      TestLabel "test_encimar4" testEncimar4,
      TestLabel "test_cuarteto" testCuarteto,
      TestLabel "test_ciclar" testCiclar,
      TestLabel "test_mapDib" test_mapDib,
      TestLabel "test_change" test_change,
      TestLabel "test_foldDib" test_foldDib,
      TestLabel "test_foldDib_2" test_foldDib_2,
      TestLabel "test_figuras" test_figuras
    ]

-- Ejecuta las pruebas
main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

-- NOTE: ¿Como funcionan las pruebas?
-- Se usa el modulo Test.HUnit de pruebas unitarias.
--
-- 1. Crear una funcion para comprar el valor esperado con el valor obtenido.
--          Se usa assertEqual :: String -> a -> a -> Assertion
--          Ejemplo:
--          assertEqual "Mensaje de error" 1 (1+0)
--
-- 2. Luego, se debe crear un TestLabel con el nombre de la prueba y la función que se encargada del unitest.
-- 3. Finalmente, se crea una lista de pruebas con todos los TestLabel.
-- 4. Se ejecutan las pruebas con runTestTT en main.

-- NOTE: ¿Como se ejecutan las pruebas?
-- Con cabal, ejecutar`cabal run dibujos_`.
-- Con ghci ejecutar los siguientes comandos:
--          0. "ghci -isrc"
--          1. ":set -package HUnit"
--          2. ":l test/TestDibujos.hs"
--          3. "main"
--          4. ":q" para salir de ghci.