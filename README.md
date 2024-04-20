---
title: Laboratorio de Funcional
author: Giménez García Daián
        Lugo Viola Ramiro
        Viola Di Benedetto Matías
---
La consigna del laboratorio está en https://tinyurl.com/funcional-2024-famaf

## 1. Tareas
Pueden usar esta checklist para indicar el avance.

## Verificación de que pueden hacer las cosas.
- [ ] Haskell instalado y testeos provistos funcionando. (En Install.md están las instrucciones para instalar.)

## 1.1. Lenguaje
- [ ] Módulo `Dibujo.hs` con el tipo `Dibujo` y combinadores. Puntos 1 a 3 de la consigna.
- [ ] Definición de funciones (esquemas) para la manipulación de dibujos.
- [ ] Módulo `Pred.hs`. Punto extra si definen predicados para transformaciones innecesarias (por ejemplo, espejar dos veces es la identidad).

## 1.2. Interpretación geométrica
- [ ] Módulo `Interp.hs`.

## 1.3. Expresión artística (Utilizar el lenguaje)
- [ ] El dibujo de `Dibujos/Feo.hs` se ve lindo.
- [ ] Módulo `Dibujos/Grilla.hs`.
- [ ] Módulo `Dibujos/Escher.hs`.
- [ ] Listado de dibujos en `Main.hs`.

## 1.4 Tests
- [ ] Tests para `Dibujo.hs`.
- [ ] Tests para `Pred.hs`.

## 2. Experiencia

## 3. Preguntas
Al responder tranformar cada pregunta en una subsección para que sea más fácil de leer.

- **`1.`** ¿Por qué están separadas las funcionalidades en los módulos indicados? Explicar detalladamente la responsabilidad de cada módulo.
- **`2.`** ¿Por qué las figuras básicas no están incluidas en la definición del lenguaje, y en vez de eso, es un parámetro del tipo?
- **`3.`** ¿Qué ventaja tiene utilizar una función de `fold` sobre hacer pattern-matching directo?
- **`4.`** ¿Cuál es la diferencia entre los predicados definidos en Pred.hs y los tests?

## Dsarrollo de las preguntas. 

**`Para empezar a redactar la experiencia que tuvimos como equipo a la hora de emprender el desarrolo de este proyecto dividamoslo en varios puntos.`**

## Experiencia en cuanto a: 

- **`1.`** Trabajo en equipo.
- **`2.`** Adaptabilidad al lenguaje funcional y salir un poco del pensamiento de ver todo como si fueran estados, 
  cosa que nos induce el echo de haber programado más en lenguajes imperativos. 
- **`3.`** Darnos cuenta de que en realidad todo es posible. 

**`1. Trabajo en equipo:`** Cuando nos presentaron el proyecto dijimos que iba a ser un proyecto desafiante, difícil tal vez en cuanto a no saber 
  ni recordar casi nada de haskell, e implemtar cosas que un principio nos parecían absurdas. 
  Una de las cosas que podemos destacar más allá de haber resuelto el lab en tiempo y forma, es justamente el trabajo en equipo. 
  Al pricipio simplemente nos poníamos por nuestra cuenta a resolver las consignas sin siquiera pensar en que el otro tambíen quería ayudar
  a resolver dichas consignas, motivo por el cuál después de varias charlas pudimos comprender la importancia de escuchar a cada integrante del grupo
  y darle su espacio para que pueda contribuír al desarrollo de lab. Entendimos que a veces vale más frenar un poco e ir más lento pero que todo el 
  equipo avance en conjunto y no cada uno por su cuenta, aprendimos también la importancia de delegar tareas y confiar en los demás integrantres del grupo
  en que van a cumplir sus tareas de manera efectiva. También, y esto es una de las cosas más importantes, apredimos que lo fundamental de todo es 
  la comunicacón y la escucha activa para poder mejorar tanto uno como persona, como también aceptar que hay veces que los demás tiene ideas mejores que uno. 

**`2. Adaptabilidad al lenguaje funcional y salir un poco del pensamiento de ver todo como si fueran estados:`** 
  Funciones que no reciben variables si no que simplemente declaran nuevas variables, adaptarnos 
  al echo de que las funciones no modifican variables globales para el bloque donde están declaradas las funciones, pasar funciones como parámetros a otras funciones, 
  no tener que preocuparnos por los memory leak, ni tener que asignar memoria para estructuras dinámicas. Todo esto rompe por decirlo de alguna manera, con lo que veníamos haciendo los años anteriores en cuanto a programación imperativa. Si bien hemos usado haskell en algunas cosas para darnos una idea de cómo era la programación funcional, nos enfrentamos a romper la manera del pensamiento imperativo y empezar a entender las cosas sin estados, y del lenguaje declarativo en sí, por un lado nos costó obviamente adaptarnos en cuanto justamente eso, pero en función mientras íbamos programando, nos dimos cuenta que es una pensamiento más lógico,abstracto, ordenado, intuitivo, y de fácil compresión en comparación con un lenguaje imperativo. 
  Las cuestiones fundamentales es que era muy fácil encontar los errores, a ser un lenguaje de tipado fuerte nos brinda esa ventaja. 
  Pasar funciones como parametros resulta muy cómodo ya que no tenemos que declarar variables como parámetros fomales de funciones, ni declarar variables para asignar valores del retorno de una función. El código en general es más legible, corto y conciso y de fácil comprensión.

  En cuanto a las deventajas detectamos la rigidez del lenguaje funcional, nos priban de hacer ciertas cosas, como por ejemplo para la entrada y salida de datos se usan mónadas, que son pequeñas "burbujas" dentro del lenguaje funcional que nos permiten programar de manera imperativa, ya que la estrada `(input)` y la salida `(output)` no se pueden hacer sin ayuda de las declaraciones imperativas. 

**`3. Darnos cuenta de que en realidad todo es posible:`** Cómo comentaba antes, en principio dijimos que el proyecto iba a ser durísimo pero en base íbamos desarrolando el lab y haciendo las consignas, el nivel de dificultad por supuesto que no era el mismo. Sino que simplemente en general uno tiende a echarse para atras cuando se nos presenta algo que pensamos que no está a nuesto alcance. Entonces nos llevamos como experiencia que cuando uno realmente se compromete con algo, irremediablemente llegan los resultados, y eso nos pasó con este proyecto. 

**`El proyecto en general fue muy divertido porque aprendimos un montón tanto en el trabajo en equipo como en el área de programación funcional en sí.`** 


# 3. Preguntas que debemos responder:

- **`1.`** ¿Por qué están separadas las funcionalidades en los módulos indicados? Explicar detalladamente la responsabilidad de cada módulo.
- **`2.`** ¿Por qué las figuras básicas no están incluidas en la definición del lenguaje, y en vez de eso, es un parámetro del tipo?
- **`3.`** ¿Qué ventaja tiene utilizar una función de `fold` sobre hacer pattern-matching directo?
- **`4.`** ¿Cuál es la diferencia entre los predicados definidos en Pred.hs y los tests?



# Respuestas del punto 3. 

**`1.`** La respuesta más obvia claramente es que las funcionalidades están implementadas en diferentes módulos para una mayor comprensión del código, una mejora práctica en el desarrollo del código y una buena implementación para poder mantener el código. 
El tener todo en distintos módulos nos brinda una tremenda ventaja en la compresión y manejo del código, ya que en general para cada error, ya sabemos rápidamente dónde ubicarnos para poder analizar detenidamente las funicones que implementa dicho módulo y hacer un uso correcto con las mismas. 
   
# Módulo `main.hs` # 

- El módulo `main` es el encargado de controlar el tamaño de la ventena o interfaz gráfica en donde se va a dibujar los dibujos que previamente tenemos la posibilidad de  elegir de una lista que nos brinda al ejecutar `main` con la opción `--lista` impriéndome una lista de dibujos para elegir cúal dibujar. 
En principio `main` importa todos los módulos con unas de las funciones que es la que va usar para llamar al módulo en es sí. 

**Por ejemplo:** `import Dibujos.Escher`. `(Escher)` importa la función `escher` del módulo `Dibujos.Escher`, en el `main`, la variable `configs` contiene una lista de todas estas funciones donde ésta lista la usará la función `initial'` que llamará a su vez a la función `initial` del módulo interp, para empezar a dibujar el dibujo. 

Esta sería la información que "guarda" la variable `configs` para nuestra implementación.
```haskell
-- Lista de configuraciones de los dibujos
configs :: [Conf]
configs = [feoConf, (grillaConf 7 7 windowSize), escher ]
```

# Módulo `Interp` # 
- El módulo `Interp` es el módulo encargado mediante la función `interp` que usa a su vez la función `foldib` de generar una abstracción y comunciación entre las funciones que usan los contructores del `data type dibujo del módulo Dibujo` y las funciones que están declaradas en éste módulo `interp`, que trabajan directamente con vectores de tipo `floantipic` del módulo `Floantipic` y no con los contructores del tipo dibujo.  

Entonces se llama a la función `initial` del módulo `Interp` teniendo en cuenta lo siguiente: 
 Dada una computación que construye una configuración, mostramos por
 pantalla la figura de la misma de acuerdo a la interpretación para
 las figuras básicas. Permitimos una computación para poder leer
 archivos, tomar argumentos, etc.

La función `initial` recibe como parámetros una configuración `(Conf)` que tiene el nombre, el dibujo y la interpetación básica. Todo esto está en la variable `Conf`, luego recibe un `float` que es el tamaño de la ventana, con esos parámetros y llamando a la función `interp` dentro de `initial` se genera por pantalla el dibujo pasado por como argumento. 

# Módulo `Floatingpic` #
En este módulo se definen los tipos: 
``` haskell
type FloatingPic = Vector -> Vector -> Vector -> Picture 
 data Conf = forall a . Conf 
  { name :: String,
    pic :: Dibujo a,
    bas :: Output a
  }
```  
que son básicamente lo que usan los demás módulos para pasar la información así poder construir los dibujos. 
Además se construye la grilla, en otras palabras, las lineas verticales y horizontales que forman el recuadro y que nos indican el lugar donde hacer nuestros dibujos. 
La función `grid` que es la encagrada de hacer justamente eso. Es llamada desde el módulo `Interp` en la función `initial`. 

Esta sería la declaración de la función que me construye la grilla: 

```haskell
-- Una grilla de n líneas, comenzando en v con una separación de sep y
-- una longitud de l.
grid :: Int -> Vector -> Float -> Float -> Picture
grid n v sep l = pictures [ls, translate 0 (l * toEnum n) (rotate 90 ls)]
  where
    ls = pictures $ take (n + 1) $ hlines v sep l
```    


# Módulo `Dibujo` #

En este módulo se declara el tipo
``` haskell
data Dibujo a = Basica a
              | Encimar (Dibujo a) (Dibujo a)
              | Apilar Float Float (Dibujo a) (Dibujo a)
              | Juntar Float Float (Dibujo a) (Dibujo a)
              | Rot45 (Dibujo a)
              | Rotar (Dibujo a)
              | Espejar (Dibujo a)
              deriving (Eq,Show)
```              

y todas sus respectivas funciones las cuales tiene la tarea de ir modificándome el dibujo de interpretación básica.
Con estas funciones podemos formar una arquitectura y complejidad de dibujo mediante la manipulación y uso adecuado para poder contruir diferentes dibujos unsando previamente una interpretación básica. Es decir, que con esta funciones armamos el dibujo, que luego será traducido con las funciones que trabajan con los vectores en el módulo `Interp` modificando la interpretación básica del dibujo. 

**Por ejemplo:** en el módulo `Escher` empezamos simplemente con el dibujo básico que es el triángulo, y usando las funciones declaradas en el módulo `dibujo` generamos otros dibujos más complejos y así hasta generar un dibujo completo que va a estar definido como un texto que luego será imprimido por pantalla. 

Dicho texto sería declarado por la siguiente función del archivo `Escher.hs`:
```haskell
noneto :: Dibujo Escher -> Dibujo Escher -> Dibujo Escher -> Dibujo Escher -> Dibujo Escher 
        -> Dibujo Escher -> Dibujo Escher -> Dibujo Escher -> Dibujo Escher -> Dibujo Escher
noneto p q r s t u v w x = apilar 1 2 (juntar 1 2 p (juntar 1 1 q r)) 
                        (apilar 1 1 (juntar 1 2 s (juntar 1 1 t u)) (juntar 1 2 v (juntar 1 1 w x)))
```
Este `"TEXTO"` es el que sería interpretado por la función `interp` del módulo `Interp`, y luego una vez interpretado ser impreso por pantalla mediante las funciones declaradas en el módulo `Interp`


En otras palabras, con las funciones del módulo `Dibujo` formamos la arquitectura del dibujo, luego la función `interp` del módulo `Interp` se encarga de traducir esas funciones que trabajan con los contructores del `data type dibujo` a las funciones que trabajan con los vectores del módulo `FloatingPic` modificando ahora sí la interpetación básica que es un triángulo. 

Como analogía agrego que las funciones del módulo `dibujo` y la construcción del dibujo sería como un plano para un edificio, la función `interp` del módulo `Interp` sería el ingeniero a cargo del proyecto en entender y traducir a los obreros que serían las funciones del Módulo `Interp` para que me construyan el dibujo. 
Dichas funciones serían: 
``` haskell
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

# Módulo `Pred` # 

**`4.`** La principal diferencia entre los predicados definidos en `Pred.hs` y los test definidos tanto en `TestDibujos.hs` y `TestPred.hs`
es que los predicados en general no prueban que las funciones contructoras del módulo `Dibujo` estén efectivamente bien construídas y que semánticamente hacen lo que realmente debieran hacer. 
En otras palabras, los predicados que están definidos en `Pred.hs` verifican que las figuras básicas cumplan un cierto predicado que pasamos como parámetro jústamente a los predicados. 
Es decir, están los predicados generales abarcativos como lo son: 

``` haskell
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

Por el contratio, los test verfician que las funciones constructoras tanto del Módulo `Pred` como del módulo `Dibujo` estén bien construídas y que semánticamente cumpla con lo que se espera, en otras palabras tenemos un test de la siguiente forma: 

```haskell
testEncimar :: Test
testEncimar =
  TestCase $
    assertEqual
      "La función encimar debería encimar dos dibujos"
      (Encimar (Basica "rectangulo") (Basica "rectangulo"))
      (encimar (Basica "rectangulo") (Basica "rectangulo"))
```

Esto es una funcón que está declarada en `TestDibujos.hs`. Acá podemos ver claramente que estamos verificando mendiante una comparación de igualdad lo que debería hacer la función `encimar`, que es simplemente devolver un dibujo de esta forma  `(Encimar (Basica "rectangulo") (Basica "rectangulo"))` en donde como mencionamos antes, verificamos que la función se comporta bien semánticamente y que hace realmente lo que se espera que haga la función.  

# 4. Extras
Completar si hacen algo.