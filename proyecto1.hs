--Verifica si un entero es igual a 0
esCero :: Int -> Bool
esCero n = n == 0

--Verifica si un entero es estrictamente mayor a 0
esPositivo :: Int -> Bool
esPositivo n = n > 0

--Verifica si un caracter es una vocal en minuscula
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
productoria [] = 1
productoria (x : xs) = x * productoria xs

--verifica si un numero se encuentra o no en la lista
pertenece :: Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x : xs) = n == x || pertenece n xs
