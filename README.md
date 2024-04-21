---
title: Laboratorio de Funcional
author: Giménez García Daián
  Lugo Viola Ramiro
  Viola Di Benedetto Matías
---

La consigna del laboratorio está en https://tinyurl.com/funcional-2024-famaf

# 1. Tareas

Pueden usar esta checklist para indicar el avance.

## Verificación de que pueden hacer las cosas.

- [x] Haskell instalado y testeos provistos funcionando. (En Install.md están las instrucciones para instalar.)

## 1.1. Lenguaje

- [x] Módulo `Dibujo.hs` con el tipo `Dibujo` y combinadores. Puntos 1 a 3 de la consigna.
- [x] Definición de funciones (esquemas) para la manipulación de dibujos.
- [x] Módulo `Pred.hs`. Punto extra si definen predicados para transformaciones innecesarias (por ejemplo, espejar dos veces es la identidad).

## 1.2. Interpretación geométrica

- [x] Módulo `Interp.hs`.

## 1.3. Expresión artística (Utilizar el lenguaje)

- [x] El dibujo de `Dibujos/Feo.hs` se ve lindo.
- [x] Módulo `Dibujos/Grilla.hs`.
- [x] Módulo `Dibujos/Escher.hs`.
- [x] Listado de dibujos en `Main.hs`.

## 1.4 Tests

- [x] Tests para `Dibujo.hs`.
- [x] Tests para `Pred.hs`.

---

# 2. Experiencia

Para empezar a redactar la experiencia que tuvimos como equipo a la hora de emprender el desarrollo de este proyecto, dividamos en varios puntos:

- **`1.`** Trabajo en equipo.
- **`2.`** Adaptabilidad al lenguaje funcional y salir un poco del pensamiento de ver todo como si fueran estados,
  cosa que nos induce el echo de haber programado más en lenguajes imperativos.
- **`3.`** Darnos cuenta de que en realidad todo es posible.

## 2.1. Trabajo en equipo

Cuando nos presentaron el proyecto dijimos que iba a ser un proyecto desafiante, difícil tal vez en cuanto a no saber
ni recordar casi nada de haskell, e implementar cosas que un principio nos parecían absurdas.
Una de las cosas que podemos destacar más allá de haber resuelto el lab en tiempo y forma, es justamente el trabajo en equipo.
Al principió simplemente nos poníamos por nuestra cuenta a resolver las consignas sin siquiera pensar en que el otro también quería ayudar
a resolver dichas consignas, motivo por el cuál después de varias charlas pudimos comprender la importancia de escuchar a cada integrante del grupo
y darle su espacio para que pueda contribuir al desarrollo de lab. Entendimos que a veces vale más frenar un poco e ir más lento pero que todo el
equipo avance en conjunto y no cada uno por su cuenta, aprendimos también la importancia de delegar tareas y confiar en los demás integrantes del grupo
en que van a cumplir sus tareas de manera efectiva. También, y esto es una de las cosas más importantes, aprendimos que lo fundamental de todo es
la comunicación y la escucha activa para poder mejorar tanto uno como persona, como también aceptar que hay veces que los demás tiene ideas mejores que uno.

## 2.2. Adaptabilidad al lenguaje funcional y salir un poco del pensamiento de ver todo como si fueran estados

Funciones que no reciben variables si no que simplemente declaran nuevas variables, adaptarnos
al echo de que las funciones no modifican variables globales para el bloque donde están declaradas las funciones, pasar funciones como parámetros a otras funciones, no tener que preocuparnos por los memory leak, ni tener que asignar memoria para estructuras dinámicas. Todo esto rompe por decirlo de alguna manera, con lo que veníamos haciendo los años anteriores en cuanto a programación imperativa. Si bien hemos usado haskell en algunas cosas para darnos una idea de cómo era la programación funcional, nos enfrentamos a romper la manera del pensamiento imperativo y empezar a entender las cosas sin estados, y del lenguaje declarativo en sí, por un lado nos costó obviamente adaptarnos en cuanto justamente eso, pero en función mientras íbamos programando, nos dimos cuenta que es una pensamiento más lógico,abstracto, ordenado, intuitivo, y de fácil compresión en comparación con un lenguaje imperativo.
Las cuestiones fundamentales es que era muy fácil encontrar los errores, a ser un lenguaje de tipado fuerte nos brinda esa ventaja.
Pasar funciones como parámetros resulta muy cómodo ya que no tenemos que declarar variables como parámetros formales de funciones, ni declarar variables para asignar valores del retorno de una función. El código en general es más legible, corto y conciso y de fácil comprensión.

En cuanto a las desventajas detectamos la rigidez del lenguaje funcional, nos privan de hacer ciertas cosas, como por ejemplo para la entrada y salida de datos se usan mónadas, que son pequeñas "burbujas" dentro del lenguaje funcional que nos permiten programar de manera imperativa, ya que la estrada `(input)` y la salida `(output)` no se pueden hacer sin ayuda de las declaraciones imperativas.

## 2.3. Darnos cuenta de que en realidad todo es posible

Cómo comentaba antes, en principio dijimos que el proyecto iba a ser durísimo pero en base íbamos desarrollando el lab y haciendo las consignas, el nivel de dificultad por supuesto que no era el mismo. Sino que simplemente en general uno tiende a echarse para atrás cuando se nos presenta algo que pensamos que no está a nuestro alcance. Entonces nos llevamos como experiencia que cuando uno realmente se compromete con algo, irremediablemente llegan los resultados, y eso nos pasó con este proyecto.

> **GENERAL**:
> El proyecto fue muy divertido porque aprendimos un montón tanto en el trabajo en equipo como en el área de programación funcional en sí.

---

# 3. Preguntas que debemos responder:

## 3.1. ¿Por qué están separadas las funcionalidades en los módulos indicados?

La respuesta más obvia claramente es que las funcionalidades están implementadas en diferentes módulos para una mayor comprensión del código, una mejora práctica en el desarrollo del código y una buena implementación para poder mantener el código.
El tener todo en distintos módulos nos brinda una tremenda ventaja en la compresión y manejo del código, ya que en general para cada error, ya sabemos rápidamente dónde ubicarnos para poder analizar detenidamente las funciones que implementa dicho módulo y hacer un uso correcto con las mismas.

## 3.1. Explicación de cada modulo:

### Módulo `main.hs`

El módulo `main` es el encargado de controlar el tamaño de la ventana o interfaz gráfica en donde se va a dibujar los dibujos que previamente tenemos la posibilidad de elegir de una lista que nos brinda al ejecutar `main` con la opción `--lista` imprimiendo una lista de dibujos para elegir cual dibujar.
En principio `main` importa todos los módulos con unas de las funciones que es la que va usar para llamar al módulo en es sí.

**Por ejemplo:** `import Dibujos.Escher`. `(Escher)` importa la función `escher` del módulo `Dibujos.Escher`, en el `main`, la variable `configs` contiene una lista de todas estas funciones donde ésta lista la usará la función `initial'` que llamará a su vez a la función `initial` del módulo `Interp`, para empezar a dibujar el dibujo.

Esta sería la información que "guarda" la variable `configs` para nuestra implementación.

```haskell
-- Lista de configuraciones de los dibujos
configs :: [Conf]
configs = [feoConf, (grillaConf 7 7 windowSize), escher ]
```

### Módulo `Interp`

El módulo `Interp` es el módulo encargado mediante la función `interp` que usa a su vez la función `foldib` de generar una abstracción y comunicación entre las funciones que usan los constructores del `data type dibujo del módulo Dibujo` y las funciones que están declaradas en éste módulo `interp`, que trabajan directamente con vectores de tipo `floantipic` del módulo `Floantipic` y no con los constructores del tipo dibujo.

Entonces se llama a la función `initial` del módulo `Interp` teniendo en cuenta lo siguiente:
Dada una computación que construye una configuración, mostramos por pantalla la figura de la misma de acuerdo a la interpretación para las figuras básicas. Permitimos una computación para poder leer archivos, tomar argumentos, etc.
La función `initial` recibe como parámetros una configuración `(Conf)` que tiene el nombre, el dibujo y la interpretación básica. Todo esto está en la variable `Conf`, luego recibe un `float` que es el tamaño de la ventana, con esos parámetros y llamando a la función `interp` dentro de `initial` se genera por pantalla el dibujo pasado por como argumento.

### Módulo `Floatingpic`

En este módulo se definen los tipos:

```haskell
type FloatingPic = Vector -> Vector -> Vector -> Picture
 data Conf = forall a . Conf
  { name :: String,
    pic :: Dibujo a,
    bas :: Output a
  }
```

que son básicamente lo que usan los demás módulos para pasar la información así poder construir los dibujos.
Además se construye la grilla, en otras palabras, las lineas verticales y horizontales que forman el recuadro y que nos indican el lugar donde hacer nuestros dibujos.
La función `grid` que es la encargada de hacer justamente eso. Es llamada desde el módulo `Interp` en la función `initial`.

Esta sería la declaración de la función que me construye la grilla:

```haskell
-- Una grilla de n líneas, comenzando en v con una separación de sep y
-- una longitud de l.
grid :: Int -> Vector -> Float -> Float -> Picture
grid n v sep l = pictures [ls, translate 0 (l * toEnum n) (rotate 90 ls)]
  where
    ls = pictures $ take (n + 1) $ hlines v sep l
```

### Módulo `Dibujo`

En este módulo se declara el tipo

```haskell
data Dibujo a = Basica a
              | Encimar (Dibujo a) (Dibujo a)
              | Apilar Float Float (Dibujo a) (Dibujo a)
              | Juntar Float Float (Dibujo a) (Dibujo a)
              | Rot45 (Dibujo a)
              | Rotar (Dibujo a)
              | Espejar (Dibujo a)
              deriving (Eq,Show)
```

y todas sus respectivas funciones las cuales tiene la tarea de ir modificando el dibujo de interpretación básica.
Con estas funciones podemos formar una arquitectura y complejidad de dibujo mediante la manipulación y uso adecuado para poder construir diferentes dibujos usando previamente una interpretación básica. Es decir, que con esta funciones armamos el dibujo, que luego será traducido con las funciones que trabajan con los vectores en el módulo `Interp` modificando la interpretación básica del dibujo.

**Por ejemplo:** en el módulo `Escher` empezamos simplemente con el dibujo básico que es el triángulo, y usando las funciones declaradas en el módulo `dibujo` generamos otros dibujos más complejos y así hasta generar un dibujo completo que va a estar definido como un texto que luego será imprimido por pantalla.

Dicho texto sería declarado por la siguiente función del archivo `Escher.hs`:

```haskell
noneto :: Dibujo Escher -> Dibujo Escher -> Dibujo Escher -> Dibujo Escher -> Dibujo Escher
        -> Dibujo Escher -> Dibujo Escher -> Dibujo Escher -> Dibujo Escher -> Dibujo Escher
noneto p q r s t u v w x = apilar 1 2 (juntar 1 2 p (juntar 1 1 q r))
                        (apilar 1 1 (juntar 1 2 s (juntar 1 1 t u)) (juntar 1 2 v (juntar 1 1 w x)))
```

Este `"TEXTO"` es el que sería interpretado por la función `interp` del módulo `Interp`, y luego una vez interpretado ser impreso por pantalla mediante las funciones declaradas en el módulo `Interp`

En otras palabras, con las funciones del módulo `Dibujo` formamos la arquitectura del dibujo, luego la función `interp` del módulo `Interp` se encarga de traducir esas funciones que trabajan con los constructores del `data type dibujo` a las funciones que trabajan con los vectores del módulo `FloatingPic` modificando ahora sí la interpretación básica que es un triángulo.

Como analogía agrego que las funciones del módulo `dibujo` y la construcción del dibujo sería como un plano para un edificio, la función `interp` del módulo `Interp` sería el ingeniero a cargo del proyecto en entender y traducir a los obreros que serían las funciones del módulo `Interp` para que me construyan el dibujo.

Dichas funciones serían:

```haskell
-- Interpretación de (^^^)
ov :: Picture -> Picture -> Picture
-- Rotar45 (b -> b)
r45 :: FloatingPic -> FloatingPic
-- Rotar (b -> b)
rot :: FloatingPic -> FloatingPic
-- Espejar (b -> b)
esp :: FloatingPic -> FloatingPic
-- Superponer/Encimar (b -> b -> b)
sup :: FloatingPic -> FloatingPic -> FloatingPic
-- Apilar (Float -> Float -> b -> b -> b)
api :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
-- Juntar  (Float -> Float -> b -> b -> b)
jun :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
```

Estas funciones serían los obreros en la analogía.

Después tenemos la siguiente función que en la analogía sería el ingeniero en jefe o arquitecto.

```haskell
-- La función interp toma una función f que puede interpretar un valor de tipo a y
-- produce una representación visual FloatingPic.
-- Queremos modificar esta función f para que pueda interpretar valores del tipo Dibujo a,
-- es decir, cambiar la interpretación para que funcione con dibujos.
interp :: Output a -> Output (Dibujo a)
```

Y luego tendríamos las funciones del módulo `Dibujo` que en la analogía serían el plano o las reglas a seguir, que son las siguientes.

```haskell
encimar :: Dibujo a -> Dibujo a -> Dibujo a
apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
rot45 :: Dibujo a -> Dibujo a
rotar :: Dibujo a -> Dibujo a
espejar :: Dibujo a -> Dibujo a
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(///) :: Dibujo a -> Dibujo a -> Dibujo a
```

### Módulo `Pred`

Hablamos de este modulo en la pregunta `3.4.` con mas profundidad.

## 3.2. ¿Por qué las figuras básicas no están incluidas en la definición del lenguaje, y en vez de eso, es un parámetro del tipo?

Tenemos que las figuras básicas no están incluidas en la definición del lenguaje, y en vez de eso, es un parámetro del tipo, porque esto nos permite abstraernos de las figuras que se pueden dibujar.
Logrando de esta forma que nuestro lenguaje es mucho mas general, no depende de figuras especificas y para usarlo podemos definir exactamente las figuras que vamos a utilizar.
Asi el lenguaje no nos limita y tampoco nos obliga a tener mas de lo que necesitamos.
Como ejemplo de esto podemos ver como en el módulo `Dibujos/Escher.hs` tenemos que las figuras básicas son simplemente triángulos mientras que en el módulo `Dibujos/Grilla.hs` las figuras básicas son una tupla de dos enteros.

## 3.3. ¿Qué ventaja tiene utilizar una función de `fold` sobre hacer pattern-matching directo?

- **`Abstraccion:`** Separamos la logica de como se manejan los diferentes casos del tipo de datos de la implementación real de esos casos.
- **`Modularidad:`** Al querer aplicar una funcion a cada parte del dibujo, simplemente necesitamos utilizar fold, en lugar de repetir directamente el codigo existente.
- **`Separacion de responsabilidades:`** De la recursion y el pattern-matching se encarga la funcion de fold, las demas solo necesitan encargarse de la implementacion de las funciones que se deben aplicar en cada caso.
- **`Simplicidad:`** En los puntos anteriores hablamos de abstraccion, modularidad y separacion de responsabilidades, estas caracteristicas hacen que el codigo sea mas facil de leer, entender y por lo tanto modificar o extender.
- **`Aprendizaje y buenas practicas:`** Utilizar funciones de alto orden, para combinar varias funciones pequeñas y crear funcionalidades más complejas, es en buena parte responsable de las ventajas dichas anteriormente y es una cualidad distintiva de los lenguajes declarativos.

## 3.4. ¿Cuál es la diferencia entre los predicados definidos en Pred.hs y los tests?

La principal diferencia entre los predicados definidos en `Pred.hs` y los test definidos tanto en `TestDibujos.hs` y `TestPred.hs`
es que los predicados en general no prueban que las funciones constructoras del módulo `Dibujo` estén efectivamente bien construidas y que semánticamente hacen lo que realmente debieran hacer.
En otras palabras, los predicados que están definidos en `Pred.hs` verifican que las figuras básicas cumplan un cierto predicado que pasamos como parámetro justamente a los predicados.
Es decir, están los predicados generales abarcativos como lo son:

```haskell
-- NOTE: Dado un predicado sobre básicas, cambiar todas las que satisfacen
--       el predicado por la figura básica indicada por el segundo argumento.
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a

-- NOTE: Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool

-- NOTE: Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool

-- NOTE: Los dos predicados se cumplen para el elemento recibido.
andP :: Pred a -> Pred a -> Pred a

-- NOTE: Algún predicado se cumple para el elemento recibido.
orP :: Pred a -> Pred a -> Pred a

-- NOTE: Falla siempre es True.
falla :: Bool
```

Y luego están los predicados que pasamos como argumentos a los predicados abarcativos, éstos en general son los predicados que deben cumplir las figuras básicas.

Por el contrario, los test verifican que las funciones constructoras tanto del Módulo `Pred` como del módulo `Dibujo` estén bien construidas y que semánticamente cumpla con lo que se espera, en otras palabras tenemos un test de la siguiente forma:

```haskell
testEncimar :: Test
testEncimar =
  TestCase $
    assertEqual
      "La función encimar debería encimar dos dibujos"
      (Encimar (Basica "rectangulo") (Basica "rectangulo"))
      (encimar (Basica "rectangulo") (Basica "rectangulo"))
```

Esto es una función que está declarada en `TestDibujos.hs`. Acá podemos ver claramente que estamos verificando mediante una comparación de igualdad lo que debería hacer la función `encimar`, que es simplemente devolver un dibujo de esta forma `(Encimar (Basica "rectangulo") (Basica "rectangulo"))` en donde como mencionamos antes, verificamos que la función se comporta bien semánticamente y que hace realmente lo que se espera que haga la función.

# 4. Extras

## 4.1 Mejoras en la **Grilla**:

### Puede ser de `n x m` no solamente `7 x 7`:

FIXME: REVISAR
Para lograr esto hicimos que la función `grillaConf` reciba dos enteros `n` y `m` que son la cantidad de lineas horizontales y verticales respectivamente, y que se encargue de configurar la grilla de `n x m`. En nuestro caso en particular la grilla es de `7 x 7`.
Esto se consigue generalizando las funciones encargadas de generar las lineas horizontales y verticales de la grilla:

```haskell
-- Generamos una línea de la grilla.
drawLineGrilla :: Int -> Int -> Float-> [Dibujo BasicaTuplas]
drawLineGrilla x 0 fontSize = [figTupla (Tupla (x, 0, fontSize))]
drawLineGrilla x y fontSize = drawLineGrilla x (y - 1) fontSize ++ [figTupla (Tupla (x, y, fontSize))]

-- Generamos la grilla x por y en un array de dibujos.
drawGrilla :: Int -> Int -> Float -> [[Dibujo BasicaTuplas]]
drawGrilla 0 y fontSize = [drawLineGrilla 0 y fontSize]
drawGrilla x y fontSize = (drawGrilla (x-1) y fontSize) ++ [(drawLineGrilla x y fontSize)]
```

Ignorando el `fontSize` por el momento, podemos ver como `drawLineGrilla` genera una linea horizontal de la grilla, y `drawGrilla` genera la grilla completa de `n x m` lineas horizontales y verticales respectivamente, en un array de dibujos.

### Se adapta a difrentes tamaños de ventana sin perder la proporción:

FIXME: REVISAR
Para lograr esto hicimos que la función `grillaConf` reciba un `float` que es el tamaño de la ventana, y que se encargue de configurar la grilla de acuerdo a ese tamaño de ventana sin perder la proporción de la grilla. Esto quiere decir que el tamaño de la tipografia y de las lineas de la grilla se calculan a partir de este valor.

Primero calculamos el tamaño de la tipografia:

```haskell
-- Calculamos el tamaño de la fuente.
calculateFontSize :: Int -> Int -> Float -> Float
calculateFontSize x y windowSize = windowSize / (((fromIntegral (x+y))/2) * 565)
```

Podemos ver como el tamaño de la fuente depende de la cantidad de lineas horizontales y verticales de la grilla, y del tamaño de la ventana.

Este valor lo vamos trasladando por todas las funciones hasta llegar a la funcion encargada efectivamente de utilizar `gloss` para dibujar la grilla. Para ello agregamos un campo mas a las tuplas logrando que este valor se mantenga en todo momento:

```haskell
--                        ( x ,  y , fontSize)
data BasicaTuplas = Tupla (Int, Int,   Float ) deriving (Show, Eq)
```

Y al momento de usar `gloss` para dibujar la grilla, usamos el valor de `fontSize` para configurar el tamaño de la tipografia:

```haskell
-- Dibujamos la tupla.
drawTextTupla :: BasicaTuplas -> Picture
drawTextTupla (Tupla (x, y, fontSize)) = scale fontSize fontSize $ text $ "(" ++ show x ++ "," ++ show y ++ ")"

-- Interpretamos la tupla para gloss con sus vectores.
--      Obs: Al final del archivo se explica un detalle de esta funcion.
interpBasicaTuplas :: Output BasicaTuplas
interpBasicaTuplas tupla (d_x, d_y) (w_x, _) (_, h_y) = translate (d_x + w_x/4) (d_y + h_y/2) $ drawTextTupla tupla
```

Obs: Como no se puede definir excatemente el tamaño de la tipografia en `gloss`, lo que hacemos es escalar el texto a partir de este valor.

---
