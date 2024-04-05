module Main (main) where

import Dibujos.Escher (escher)
import Dibujos.Grilla (grillaConf)
import Dibujos.Feo (feoConf)
--import Dibujos.Cuadrados(cuadConf)
import FloatingPic (Conf (..))
import Interp (initial)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (when)
import InterpHaha (ConfH, simpleHaha, initialH')
import InterpSVG (ConfSVG, initialSVG', simpleSVG)

-- Tamaño de la ventana.
windowSize :: Float
windowSize = 800

-- Lista de configuraciones de los dibujos
configs :: [Conf]
configs = [feoConf, (grillaConf 7 7 windowSize), escher ]

configsH :: [ConfH]
configsH = map (\(Conf n p _) -> simpleHaha n p) configs

configsSVG :: [ConfSVG]
configsSVG = map (\(Conf n p _) -> simpleSVG n p) configs

-- Dibuja el dibujo n
initial' :: [Conf] -> String -> IO ()
initial' [] n = do
  putStrLn $ "No hay un dibujo llamado " ++ n
initial' (c : cs) n =
  if n == name c
    then
      initial c windowSize
    else
      initial' cs n

main :: IO ()
main = do
  args <- getArgs
  when (length args > 2 || null args) $ do
    putStrLn "Sólo puede elegir un dibujo. Para ver los dibujos use -l ."
    exitFailure
  when (head args == "-l") $ do
    putStrLn "Los dibujos disponibles son:"
    mapM_ (putStrLn . name) configs
    exitSuccess
  when (head args == "-a" && not (null $ tail args)) $ do
    initialH' configsH (args!!1) 
    exitSuccess
  when (head args == "-s" && not (null $ tail args)) $ do
    initialSVG' configsSVG (args!!1) 
    exitSuccess
  initial' configs $ head args


-- NOTE: Los siguientes comandos, hace que funcione el main.
-- ghc -package containers -package haha -package text -package lucid-svg -o mi_programa Main.hs
-- ./mi_programa -l // Para ver los dibujos disponibles pj, me va a dar una lista de dibujos [Feo, ..., ..., etc]
-- ./mi_programa Feo // ejecuta el archivo.

-- NOTE: Otra compilacion que es la mejor, pero les puse las anteriores para que entiendan un poco que es lo
--       que se está ejecutando, es la siguiente.
--       Dentro de la carpeta (paradigmas-24-lab-1-g45) ejecuten esta linea. --| cabal run dibujos Feo |--

--       Pj las siguientes lineas son dependecias para que Haskell pueda compilar los archivos armoniosamente
--       haha ^>=0.3.1.1,
--       containers ^>=0.6.8,
--       lucid-svg ^>=0.7.1.1,
--       text ^>=2.0.2

-- NOTE: Otra cosa es que cabal, prácticamente es como Makefile pero para haskell, construyendo todas las
--       dependecias entre haskell y las bibliotecas.

-- NOTE: Para que esto anduviera, tuve que borrar: 
--       --| import Dibujos.Ejemplo (ejemploConf) |-- /Main.hs line 3
--       --| import Dibujos.Cuadrados(cuadConf) |-- /Main.hs line 5
--       Del archivo dibujos.cabal, tuve que borrar:
--       --| Dibujos.Ejemplo |-- /dibujos.cabal line 63
--       --| Dibujos.Cuadrados |-- /dibujos.cabal line 66
--       También modifiqué --| configs = [feoConf] |-- /Main.hs line 16
--       Antes estaba así --| configs = [feoConf, ejemploConf, cuadConf 3] |-- /Main.hs -> line 16
--       que justamene son las funciones de los modulos --| Dibujos.Ejemplo, Dibujos.Feo, Dibujos.Cuadrado |-- /Main.hs line 3-5