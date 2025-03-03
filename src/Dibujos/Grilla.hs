module Dibujos.Grilla where


--NOTE: Importamos las librerías y módulos necesarios para construir nuestras funciones.
-----------------------------------------------------------------------------------------------------------------------
import Dibujo
import FloatingPic
import Graphics.Gloss ( Picture, scale, text, translate)
-----------------------------------------------------------------------------------------------------------------------



--NOTE: Definiciones de tipos y constructores de tipos.
-----------------------------------------------------------------------------------------------------------------------
-- Resumen del tipo       ( x ,  y , fontSize) 
data BasicaTuplas = Tupla (Int, Int,   Float ) deriving (Show, Eq)
-----------------------------------------------------------------------------------------------------------------------



--NOTE: Estas funciones son las encargada de construir la tupla y pasarle como parámetro al interp.hs.
---------------------------------------------------------------------------------------------------------------------

-- Dibujamos la tupla.
drawTextTupla :: BasicaTuplas -> Picture
drawTextTupla (Tupla (x, y, fontSize)) = scale fontSize fontSize $ text $ "(" ++ show x ++ "," ++ show y ++ ")"

-- Interpretamos la tupla para gloss con sus vectores.
--      Obs: Al final del archivo se explica un detalle de esta funcion.
interpBasicaTuplas :: Output BasicaTuplas
interpBasicaTuplas tupla (d_x, d_y) (w_x, _) (_, h_y) = translate (d_x + w_x/4) (d_y + h_y/2) $ drawTextTupla tupla



--NOTE: Funcion que modularizan las funciones constructoras.
-----------------------------------------------------------------------------------------------------------------------
-- Generamos la figura a partir de una tupla.
figTupla :: BasicaTuplas -> Dibujo BasicaTuplas
figTupla b = figura b
-----------------------------------------------------------------------------------------------------------------------



--NOTE: Las funciones que siguen, son las constructoras de la grilla.
-----------------------------------------------------------------------------------------------------------------------
-- Generamos una línea de la grilla.
drawLineGrilla :: Int -> Int -> Float-> [Dibujo BasicaTuplas]
drawLineGrilla x 0 fontSize = [figTupla (Tupla (x, 0, fontSize))]
drawLineGrilla x y fontSize = drawLineGrilla x (y - 1) fontSize ++ [figTupla (Tupla (x, y, fontSize))]

-- Generamos la grilla x por y en un array de dibujos.
drawGrilla :: Int -> Int -> Float -> [[Dibujo BasicaTuplas]]
drawGrilla 0 y fontSize = [drawLineGrilla 0 y fontSize]
drawGrilla x y fontSize = (drawGrilla (x-1) y fontSize) ++ [(drawLineGrilla x y fontSize)]
-----------------------------------------------------------------------------------------------------------------------


--NOTE: Funciones para construir la grilla a partir de las listas.
-----------------------------------------------------------------------------------------------------------------------
-- Generamos una fila a partir de un arreglo.
row :: [Dibujo a] -> Dibujo a
row [] = error "row: no puede ser vacío"
row [d] = d
row (d:ds) = juntar 1 (fromIntegral $ length ds) d (row ds)

-- Generamos una columna a partir de un arreglo.
column :: [Dibujo a] -> Dibujo a
column [] = error "column: no puede ser vacío"
column [d] = d
column (d:ds) = apilar 1 (fromIntegral $ length ds) d (column ds)
-----------------------------------------------------------------------------------------------------------------------



--NOTE: Funciones para construir la grilla, calculado el tamaño de fuente correspondiente a la ventana.
-----------------------------------------------------------------------------------------------------------------------
-- Generamos la grilla a partir de un arreglo.
grilla :: [[Dibujo a]] -> Dibujo a
grilla = column . map row

-- Generamos la grilla x por y.
grillaOut :: Int -> Int -> Float-> Dibujo BasicaTuplas
grillaOut x y fontSize = grilla (drawGrilla x y fontSize)

-- Calculamos el tamaño de la fuente.
calculateFontSize :: Int -> Int -> Float -> Float
calculateFontSize x y windowSize = windowSize / (((fromIntegral (x+y))/2) * 565)
-----------------------------------------------------------------------------------------------------------------------



--NOTE: Función encargada de pasar toda la información a interp.hs y main.hs
-----------------------------------------------------------------------------------------------------------------------
-- Exportamos la configuración de la grilla.
--      Input: 
--          x: tamaño de la grilla en x.
--          y: tamaño de la grilla en y.
--          windowSize: tamaño de la ventana.
--      Obs: El fontSize depende del tamaño de la ventana y la grilla.
grillaConf :: Int -> Int -> Float-> Conf
grillaConf x y windowSize = Conf {
     name = "Grilla"
    ,pic = grillaOut x y (calculateFontSize x y windowSize)
    ,bas = interpBasicaTuplas
}
-----------------------------------------------------------------------------------------------------------------------



-- NOTE: Para compilar primero estar en la carpeta "/paradigmas-24-lab-1-g45" 
--       y ejecutar "cabal run dibujos Grilla"
--       También pueden ejecutar "cabal run dibujos -- --lista" para ver todos 
--       los dibujos en una lista y elegir alguno para dibujar. 


-- NOTE:  Lo que sigue corrige un error que aparece en el dibujo del pdf que nos pasaron los profes.
--        
--        Objetivo: Centraliza de manera más prolija el dibujo de la grilla. 
--
--        Este error corregido está en la función --| interpBasicaTuplas |-- line 57. 
--
--        Explicación de por qué hay que dividir por 4. InterbasicaTuplas recibe como parámetros
--        (0, 0) (size, 0) (0, size) donde size es el tamaño de la ventana. Estos parámetros los recibe
--        en Interp.hs.
--        (0, 0) = (d_x, d_y)
--        (size, 0) = (w_x, _)
--        (0, size) = (_, h_y)
--        Supongamos un eje cartesiano (x,y). Donde tenemos las siguiente coordendas:
--        (0,0) punto origen
--        (4,0) punto anchura
--        (0,4) punto altura
--        Es decir, tenemos un vector que va desde el punto (0,0) al punto (4,4) en el eje cartesiano, 
--        por lo que si dividimos por 2 el punto (4,4) tendriamos (2,2) es decir tendriamos 
--        un vector que va desde el punto (0,0) al punto (2,2) y es justamente ahí donde está el problema, 
--        ya que tendriamos la misma altura y anchura.
--        Entonces dividiendo el punto (4,0) por el escalar 4 obtenndríamos el punto (1,0)
--        que justamente es la mitad de lo que se mueve el punto (0,2).
--        En otras palabras, si sumamos los puntos (1,0) + (0,2) tendriamos un vector que va
--        desde (0,0) a (1,2), es decir que empezaríamos a dibujar en el plano en el punto (1,2)
--        por supuesto se traspola a lo siguiente (d_x + w_x/4, d_y + h_y/2) que justamente harían lo mismo que (1,2s)
--        En donde w_x = h_y = size.