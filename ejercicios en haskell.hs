--1.Determina el resultado de un número x elevado a una potencia n 
  potencia :: Num a => a -> Int -> a
  potencia x n = if n==0 then 1
           else x * potencia x (n-1)

--2.Determina si un número n se encuentra en un rango determinado 

--3.Dado un número entero en segundos, determinar la cantidad de horas, minutos y segundos que contiene. 
  segundosn :: Integer -> (Integer,Integer,Integer)
  segundosn s = (horas, minutos, segundos)
     where 
         horas = div s 3600
         ss = mod s 3600
         minutos = div ss 60
         segundos = mod ss 60

--4.Determine el mayor de 4 enteros
  numero :: Integer -> Integer -> Integer
  numero x y = div ((x + y) + abs(x - y)) 2
  mayor :: Integer -> Integer -> Integer -> Integer -> Integer
  mayor w x y z = numero (numero w x) (numero y z)

--5.Calcula la suma de una lista (arreglo) de elementos. 
  sumarLista::[Int]->Int
  sumarLista [ ] = 0
  sumarLista (x:xs) = x + sumarLista(xs)

--6.Determina si un elemento dado está contenido en una lista. Devuelve verdadero o falso.
  existeElemento :: Eq a => a -> [a] -> Bool
  existeElemento _ [] = False
  existeElemento x (y:ys) = (x==y) || existeElemento x ys 

--7.Determina si dada una lista, ésta se encuentra ordenada. Se debe devolver verdadero o falso.
  listaOrdenada :: Ord a => [a] -> Bool
  listaOrdenada [] = True
  listaOrdenada [_] = True
  listaOrdenada (x:y:xs) = (x <= y) && listaOrdenada (y:xs) 

--8.Dadas dos listas, determine si son iguales. Devolver verdadero o falso.
  listaIgual :: Eq a => [a] -> [a] -> Bool
  listaIgual [] [] = True
  listaIgual (x:xs) (y:ys) = x==y && listaIgual xs ys
  listaIgual _ _ = False

--9.Realizar una función recursiva que retorne como salida el resultado de la suma 1 + 3 + 5 + 7 + 9 + N
  sumaNumeros :: Num a => [a] -> a
  sumaNumeros [] = 0
  sumaNumeros (x:xs) = x + sumaNumeros xs

--10. Realizar una función que reciba una lista y devuelva empleando recursividad otra lista de elementos pares.
  paresRecursivo :: [Integer] -> [Integer]
  paresRecursivo [] = []
  paresRecursivo (x:xs) | even x = x : paresRecursivo xs|otherwise = paresRecursivo xs

--11. Realizar una función en Haskell que permita calcular la unión, intersección y diferencia de dos conjuntos de datos. Para esto puede hacer uso de la librería "Data.set"
  --importar libreria
  import Data.Set as S
  --Union
  union :: Ord a => Set a -> Set a -> Set a
  union = union

  unionG:: Ord a => [Set a] -> Set a
  unionG = unions
 
  --Interseccion 
  interseccion :: Ord a => Set a -> Set a -> Set a
  interseccion = intersection
 
  interseccionG:: Ord a => [Set a] -> Set a
  interseccionG [c] = c
  interseccionG (cs:css) = intersection cs (interseccionG css)

  interseccionG2 :: Ord a => [Set a] -> Set a
  interseccionG2 = foldr1 interseccion 
 
  --Diferencia
  diferencia :: Ord a => Set a -> Set a -> Set a
  diferencia = difference

--12. Realizar una función que permita definir un mapa de datos y permita encontrar un valor a partir de su clave. Para esto puede hacer uso de la librería "Data.map"
  import qualified Data.Map as M
  type MultiConj a = M.Map a Int
  --definir mapa de datos vacio
  vacio :: MultiConj a
  vacio = M.empty
  --definir mapa de datos conelementos
  listaAmc:: a -> MultiConj a
  listaAmc x = M.singleton x 1
  --insertar datos
  inserta :: Ord a => a -> MultiConj a -> MultiConj a
  inserta x = M.insertWith (+) x 1
  --buscar un elemento con la clave
  pertenece :: Ord a => a -> MultiConj a -> Bool
  pertenece = M.member