module Main (main) where
import Test.HUnit    
import Dibujo 
import qualified System.Exit as Exit

-- Prueba para la función figura.
testFigura :: Test
testFigura = TestCase $ assertEqual
    "La función figura debería crear un dibujo básico"
    (Basica "círculo")
    (figura "círculo")


-- Prueba para la función encimar.
testEncimar :: Test
testEncimar = TestCase $ assertEqual
    "La función encimar debería encimar dos dibujos"
    (Encimar (Basica "rectangulo") (Basica "rectangulo"))
    (encimar (Basica "rectangulo") (Basica "rectangulo"))


-- Prueba para la función apilar.
testApilar :: Test
testApilar = TestCase $ assertEqual
    "La función apilar debería apilar dos dibujos"
    (Apilar 3 4 (Basica "rectangulo") (Basica "rectangulo"))
    (apilar 3 4 (Basica "rectangulo") (Basica "rectangulo"))


-- Prueba para la función rot45.
testRot45 :: Test
testRot45 = TestCase $ assertEqual
    "La función rot45 debería rotarme el dibujo a 45º"
    (Rot45 (Basica "rectangulo"))
    (rot45 (Basica "rectangulo"))


-- Prueba para la función rotar.
testRotar :: Test
testRotar = TestCase $ assertEqual
    "La función rotar debería rotarme el dibujo sobre el eje central"
    (Rotar (Basica "rectangulo"))
    (rotar (Basica "rectangulo"))


-- Prueba para la función espejar.
testEspejar :: Test
testEspejar = TestCase $ assertEqual
    "La función espejar debería espejarme el dibujo"
    (Espejar (Basica "rectangulo"))
    (espejar (Basica "rectangulo"))    


-- Prueba para la función (^^^).
testSuperponer:: Test
testSuperponer = TestCase $ assertEqual
    "La función (^^^)  debería superponer dos dibujos"
    (Encimar (Basica "rectangulo") (Basica "rectangulo"))
    ((^^^) (Basica "rectangulo") (Basica "rectangulo"))    


-- Prueba para la función (.-.).
testApilar_2:: Test
testApilar_2 = TestCase $ assertEqual
    "La función (.-.)  debería apilar dos dibujos"
    (Apilar 1 1 (Basica "rectangulo") (Basica "rectangulo"))
    ((.-.) (Basica "rectangulo") (Basica "rectangulo")) 


-- Prueba para la función (///).
testJuntar:: Test
testJuntar = TestCase $ assertEqual
    "La función (///)  debería juntar dos dibujos"
    (Juntar 1 1 (Basica "rectangulo") (Basica "rectangulo"))
    ((///) (Basica "rectangulo") (Basica "rectangulo")) 

-- Prueba para la función r90.
testR90:: Test
testR90 = TestCase $ assertEqual
    "La función r90  debería componer dos veces rot45 en el dibujo"
    (Rot45 (Rot45 (Basica "rectangulo")))
    (r90 (Basica "rectangulo"))

-- Prueba para la función r180.
testR180:: Test
testR180 = TestCase $ assertEqual
    "La función r180  debería componer dos veces rot90 en el dibujo"
    (Rot45 (Rot45 (Rot45 (Rot45 (Basica "rectangulo")))))
    (r180 (Basica "rectangulo")) 


-- Prueba para la función r270.
testR270:: Test
testR270 = TestCase $ assertEqual
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
testEncimar4 = TestCase $ assertEqual
    "La función encimar4 debería encimar el dibujo 4 veces rotando 90º,180º,270º"
    (Encimar (Basica "rectangulo") (Encimar rot_2_45 (Encimar rot_4_45 rot_6_45)))
    (encimar4 (Basica "rectangulo"))


-- Prueba para la función cuarteto.
testCuarteto :: Test
testCuarteto = TestCase $ assertEqual
    "La función cuarteto deberia hacer cuatro dibujos en un cuadrante."
    (Apilar 1 1 (Juntar 1 1 (Basica "rectangulo")  (Basica "rectangulo"))
    (Juntar 1 1 (Basica "rectangulo") (Basica "rectangulo")))
    (cuarteto (Basica "rectangulo") (Basica "rectangulo") (Basica "rectangulo") (Basica "rectangulo"))    


-- Prueba para la función ciclar.
testCiclar:: Test
testCiclar = TestCase $ assertEqual
    "La función ciclar debería hacer un cuarteto donde se repite la imagen"
    (Apilar 1 1 (Juntar 1 1 (Basica "rectangulo") rot_2_45)
    (Juntar 1 1 rot_4_45 rot_6_45))
    (ciclar (Basica "rectangulo"))


-- Lista de todas las pruebas
tests :: Test
tests = TestList [ TestLabel "testFigura" testFigura
                 , TestLabel "testEncimar" testEncimar
                 , TestLabel "testApilar" testApilar
                 , TestLabel "testRot45" testRot45
                 , TestLabel "testRotar" testRotar
                 , TestLabel "testEspejar" testEspejar
                 , TestLabel "testSuperponer" testSuperponer
                 , TestLabel "testApilar_2" testApilar_2
                 , TestLabel "testJuntar" testJuntar
                 , TestLabel "testR90" testR90
                 , TestLabel "testR180" testR180
                 , TestLabel "testR270" testR270
                 , TestLabel "testEncimar4" testEncimar4
                 , TestLabel "testCuarteto" testCuarteto
                 , TestLabel "testCiclar" testCiclar
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