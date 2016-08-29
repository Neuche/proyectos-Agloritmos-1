--Verifica si un entero es igual a 0
esCero :: Int -> Bool
esCero n = n == 0

--Verifica si un entero es estrictamente mayor a 0
esPositivo :: Int -> Bool
esPositivo n = n > 0

--Verifica si un caracter es una vocal en minuscula
--chequear que onda con la igualdad si acepta MAYUSCULAS.
esVocal :: Char -> Bool
esVocal n = n == 'a' || n == 'e' || n == 'i' || n == 'o' || n == 'u'

--Verifica que todos los elementos de una lista sean True
paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x : xs) = x == True && paratodo xs

--Devuelve la suma de todos los elementos de la lista
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x : xs) = x + sumatoria xs

--Devuelve el producto de todos los elementos de la lista
productoria :: [Int] -> Int
productoria [] = 0
--me parece que hay que agregar un caso mas porque la lista vacia no deberia de devolver un uno.
productoria [x] = x
productoria (x : xs) = x * productoria xs

--verifica si un numero se encuentra o no en la lista
--preguntar si esta funcion se detiene una vez que encuentra el elemento, si no mejorar.
pertenece :: Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x : xs) = n == x || pertenece n xs

--dada  una  lista  de  valores xs de tipo [a] y un predicado t::a -> Bool,
-- determina si todos los elementos en xs hacen verdadero el predicado t
paratodo':: [a] -> (a -> Bool) -> Bool
paratodo' [] termino = False
paratodo' [x] termino = termino x
paratodo' (x:xs)  termino = termino x && paratodo' xs termino

--Dada una lista de valores xs de tipo [a] y un predicado t::a ->Bool, determina
-- si algun elemento en xs hace verdadero el predicado t.

existe' :: [a] -> (a -> Bool) -> Bool
existe' [] termino = False
existe' [x] termino = termino x
existe' (x:xs) termino = termino x || existe' xs termino

--Dada una lista de valores xs de tipo [a] y una funcion t::a -> Int,(toma elementos de
-- de a y devuelve enteros), calcula la suma de la aplicacion de f a los elementos en xs

sumatoria' :: [Num] -> (a -> Num) -> Num
sumatoria' [] func = 0
sumatoria' [x] func = func x
sumatoria' (x:xs) func = func x + sumatoria' xs func

--Dada una lista de valores xs de tipo [a] y una funcion f::a -> Int, calcula el producto
--de la aplicacion de f a los elementos de xs.

productoria' :: [Num] -> (A -> Num) -> Num
productoria' [] func = 0
productoria' [x] func = func x
productoria' (x:xs) func = func x * productoria' xs func

--Definir nuevamente la funcion paratodo, pero esta vez sin usar recursion
-- o analisis de casos (((QUE HAY QUE HACER ACA???)))

paratodo'' :: [a] -> (a -> Bool) -> Bool
paratodo'' [a] func = paratodo' [a] func
