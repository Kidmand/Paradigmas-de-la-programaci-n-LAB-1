module Main (main) where
import Test.HUnit
import Pred
import Dibujo 
import qualified System.Exit as Exit

--------------------------------------------------------------------------------
-- NOTE: Predicados auxiliares:
predEsCirculo :: Pred String
predEsCirculo x = (x == "circulo")

predEsCuadrado :: Pred String
predEsCuadrado x = (x == "cuadrado")

predEsTriangulo :: Pred String
predEsTriangulo x = (x == "triangulo")
--------------------------------------------------------------------------------

auxTestCambiar :: String -> Dibujo String
auxTestCambiar "circulo" = Basica "cuadrado"
auxTestCambiar x = Basica x

-- Pruebas para la funcion cambiar.
testCambiar_1_1 :: Test
testCambiar_1_1 = TestCase $ assertEqual
    "La función cambiar en este caso debería cambiar el circulo por un cuadrado"
    (cambiar predEsCirculo auxTestCambiar (Basica "circulo"))
    (Basica "cuadrado")

testCambiar_1_2 :: Test
testCambiar_1_2 = TestCase $ assertEqual
    "La función cambiar no debería cambiar el triangulo"
    (cambiar predEsCirculo auxTestCambiar (Basica "triangulo"))
    (Basica "triangulo")

testCambiar_2_1 :: Test
testCambiar_2_1 = TestCase $ assertEqual
    "La función cambiar en este caso debería cambiar los circulos por cuadrados"
    (cambiar predEsCirculo auxTestCambiar (Apilar 1 2 (Basica "circulo") (Espejar (Basica "circulo"))))
    (Apilar 1 2 (Basica "cuadrado") (Espejar (Basica "cuadrado")))

testCambiar_2_2 :: Test
testCambiar_2_2 = TestCase $ assertEqual
    "La función cambiar debería cambiar el circulo por cuadrado y no hacer nada con el triangulo"
    (cambiar predEsCirculo auxTestCambiar (Apilar 1 2 (Basica "triangulo") (Espejar (Basica "circulo"))))
    (Apilar 1 2 (Basica "triangulo") (Espejar (Basica "cuadrado")))

testCambiar_3_1 :: Test
testCambiar_3_1 = TestCase $ assertEqual
    "La función cambiar en este caso debería cambiar los circulos por cuadrados"
    (cambiar predEsCirculo auxTestCambiar (Juntar 1 2 (Basica "circulo") (Rot45 (Basica "circulo"))))
    (Juntar 1 2 (Basica "cuadrado") (Rot45 (Basica "cuadrado")))

testCambiar_3_2 :: Test
testCambiar_3_2 = TestCase $ assertEqual
    "La función cambiar debería cambiar el circulo por cuadrado y no hacer nada con el triangulo"
    (cambiar predEsCirculo auxTestCambiar (Juntar 1 2 (Basica "circulo") (Rot45 (Basica "triangulo"))))
    (Juntar 1 2 (Basica "cuadrado") (Rot45 (Basica "triangulo")))

testCambiar_4_1 :: Test
testCambiar_4_1 = TestCase $ assertEqual
    "La función cambiar en este caso debería cambiar los circulos por cuadrados"
    (cambiar predEsCirculo auxTestCambiar (Encimar (Basica "circulo") (Rotar (Basica "circulo"))))
    (Encimar (Basica "cuadrado") (Rotar (Basica "cuadrado")))

testCambiar_4_2 :: Test
testCambiar_4_2 = TestCase $ assertEqual
    "La función cambiar en este caso no debería cambiar nada"
    (cambiar predEsCirculo auxTestCambiar (Encimar (Basica "triangulo") (Rotar (Basica "triangulo"))))
    (Encimar (Basica "triangulo") (Rotar (Basica "triangulo")))

-- Pruebas para anyDib.
testAnyDib_1 :: Test
testAnyDib_1 = TestCase $ assertEqual
    "La función anyDib debería devolver True"
    (anyDib predEsCirculo (Juntar 1 2 (Basica "circulo") (Rot45 (Basica "triangulo"))))
    (True)

testAnyDib_2 :: Test
testAnyDib_2 = TestCase $ assertEqual
    "La función anyDib debería devolver False"
    (anyDib predEsCirculo (Encimar (Basica "triangulo") (Rotar (Basica "triangulo"))))
    (False)

-- Pruebas para allDib.
testAllDib_1 :: Test
testAllDib_1 = TestCase $ assertEqual
    "La función allDib debería devolver True"
    (allDib predEsCirculo (Juntar 1 2 (Basica "circulo") (Rot45 (Basica "circulo"))))
    (True)

testAllDib_2 :: Test
testAllDib_2 = TestCase $ assertEqual
    "La función allDib debería devolver False"
    (allDib predEsCirculo (Encimar (Basica "circulo") (Rot45 (Basica "triangulo"))))
    (False)


-- NOTE: Estas pruebas dependen que allDib y anyDib funcionen correctamente.
-- Pruebas para andPred.
testAndPred_1 :: Test
testAndPred_1 = TestCase $ assertEqual
    "La función andPred debería devolver True"
    (allDib (andP predEsCirculo predEsCirculo) (Juntar 1 2 (Basica "circulo") (Rot45 (Basica "circulo"))))
    (True)

testAndPred_2 :: Test
testAndPred_2 = TestCase $ assertEqual
    "La función andPred debería devolver False"
    (allDib (andP predEsCirculo predEsCuadrado) (Juntar 1 2 (Basica "circulo") (Rot45 (Basica "triangulo"))))
    (False)

-- Pruebas para orPred.
testOrPred_1 :: Test
testOrPred_1 = TestCase $ assertEqual
    "La función orPred debería devolver True"
    (anyDib (orP predEsCirculo predEsCuadrado) (Juntar 1 2 (Basica "circulo") (Rot45 (Basica "triangulo"))))
    (True)

testOrPred_2 :: Test
testOrPred_2 = TestCase $ assertEqual
    "La función orPred debería devolver False"
    (anyDib (orP predEsCirculo predEsCuadrado) (Juntar 1 2 (Basica "triangulo") (Rot45 (Basica "triangulo"))))
    (False)


-- Lista de todas las pruebas
tests :: Test
tests = TestList [    TestLabel "testCambiar_1_1" testCambiar_1_1
                    , TestLabel "testCambiar_1_2" testCambiar_1_2
                    , TestLabel "testCambiar_2_1" testCambiar_2_1
                    , TestLabel "testCambiar_2_2" testCambiar_2_2
                    , TestLabel "testCambiar_3_1" testCambiar_3_1
                    , TestLabel "testCambiar_3_2" testCambiar_3_2
                    , TestLabel "testCambiar_4_1" testCambiar_4_1
                    , TestLabel "testCambiar_4_2" testCambiar_4_2
                    , TestLabel "testAnyDib_1" testAnyDib_1
                    , TestLabel "testAnyDib_2" testAnyDib_2
                    , TestLabel "testAllDib_1" testAllDib_1
                    , TestLabel "testAllDib_2" testAllDib_2
                    , TestLabel "testAndPred_1" testAndPred_1
                    , TestLabel "testAndPred_2" testAndPred_2
                    , TestLabel "testOrPred_1" testOrPred_1
                    , TestLabel "testOrPred_2" testOrPred_2
                 ]

-- Ejecuta las pruebas
main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

-- NOTE: ¿Cómo ejecutar?
-- Con gloss: "cabal run predicados"