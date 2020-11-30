
module Hoja4 where

import Data.Char;
import Data.List;

--A)Ordenar elementos comparables con el algoritmo quicksort
quickSort::(Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = menoresOrdenados ++ [x] ++ mayoresOrdenados
        where
            menoresOrdenados = quickSort [a | a <- xs, a <= x]
            mayoresOrdenados = quickSort [b | b <- xs, b > x]
            
--B) Dado un numero divisible y una lista de enteros divida ese numero por cada uno de los elementos ocntenido en la lista
division::(Fractional a, Eq a)=>a->a->Maybe a
division _ 0 = Nothing
division n d = Just (n/d)

--C)Mostrar todo un arbol
data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show

mostrarArbol::(Show a)->Arbol a->String
mostrarArbol (Rama AV r AV) = "("++ show r ++")"
mostrarArbol AV = "()"
mostrarArbol (Rama i r d) = "("++ mostrarArbol i ++"|-" ++ show r ++ "-|" ++ mostrarArbol d ++ ")" 

--D)Dado un arbol binario sacar el espejo del arbol
data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show

espejo::Arbol a -> Arbol a
espejo AV = AV
espejo (Rama i r d) = (Rama (espejo d) r (espejo i)

--E)
data Titulacion = GradoII | GradoIIADE | GradoADE deriving (Eq, Show)
data Estudiante = Est String Titulacion
data ListaEstudiantes = [Estudiante]
data ListaAsociados = [Estudiante]

instance Eq Estudiante where
    Est x1 y1 == Est x2 y2 = x1 == x2 && y1 == y2 

instance Show Estudiante where
    show Est x y = "(" ++ x ++ "," show y ++ ")"

mostrarAlumnosAsociaciones::(ListaAsociados,ListaEstudiantes)->ListaEstudiantes
mostrarAlumnosAsociaciones (l1,l2) = foldr(\e acum -> if existeLista e l2) then e:acum else acum) [] l1

existeLista::Estudiante-> ListaEstudiantesAsociaciones->Bool
existeLista _ [] = False
existeLista a l = foldr(\e acum-> if(a==e) then True else acum ) False l

--F)Representar Fecha
data Fecha = Fe Int Int Int

instance Show Fecha where
    show (Fecha x y z) = show x ++ "/" ++ show y ++ "/" ++ show z

--G)Comparar dos fechas iguales
instance Eq Fecha where
    (Fecha x1 y1 z1) == (Fecha x2 y2 z2) = (x1 == x2) && (y1 == y2) && (z1 == z2)
    
mismaFecha::Fecha->Fecha->Bool
mismaFecha f1 f2 = f1==f2

--H)
instance Ord Fecha where
    (Fecha x1 y1 z1) <= (Fecha x2 y2 z2) = if (z1 < z2) then True else if (z1 > z2) then False else if (y1 < y2) then True else if (y1 > y2) then False else if (x1 < x2) then True else if (x1 > x2) then False else True    
    (Fecha x1 y1 z1) > (Fecha x2 y2 z2) = if (z1 > z2) then True else if (z1 < z2) then False else if (y1 > y2) then True else if (y1 < y2) then False else if (x1 > x2) then True else if (x1 < x2) then False else False
    (Fecha x1 y1 z1) == (Fecha x2 y2 z2) = (x1 == x2) && (y2 == y2) && (z1 == z2) 
    
quickSortFecha::[Fecha]->[Fecha]
quickSortFecha a = quickSort a

--I)Clase coleccion y metodos
data Coleccion c where
    esVacia:: c a -> Bool
    insertar:: c a -> a -> c a
    primero:: c a -> a
    eliminar:: c a -> c a
    size:: c a -> Int

data Pila a = Pil [a] deriving Show
data Cola a = Col [a] deriving Show

instance Coleccion Pila where
    esVacia (Pil x) = length x == 0
    insertar (Pil x) e = Pil (e:x)
    primero (Pil (x:xs)) = x
    eliminar (Pil (x:xs)) = Pil (xs)
    size (Pil x) = length x

instance Coleccion Cola where
    esVacia (Cola x) = lenght = 0
    insertar (Col x) e =  Col (x++[e])
    primero (Col (x:xs)) = x
    eliminar (Col (x:xs)) = Col (xs)
    size (Col x) = length x