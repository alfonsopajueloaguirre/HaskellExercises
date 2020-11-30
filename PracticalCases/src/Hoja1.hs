
module Hoja1 where
import Data.Char;


--A)Dado tres numeros comprobar que estan ordenados de menor a mayor
estaOrdenado::(Int,Int,Int)->Bool
estaOrdenado (x,y,z) = if (x<=y)&&(y<=z) then True else False

--B)Ordenado de menor a mayor 3 numeros
ordenar::(Int,Int,Int)->(Int,Int,Int)
ordenar(x,y,z)
		|estaOrdenado(x,y,z) = (x,y,z)
		|(x<y)=if z<=x then(z,x,y)else(x,z,y)
		|z>=x=(y,x,z)
		|otherwise=if z>=y then(y,z,x)else(z,y,x)
		
--C)Recibe numero real y devuelve una tupla con su parte entera y sus primeros decimales
devuelveReal::Float->(Int,Int)
devuelveReal  r = (truncate(r),truncate(r*100)-100*truncate(r))

--D)Recibe radio de circunferencia y devuelve 2 tuplas con longitud y area del circulo) usando where y no pi
calcularCirculo::Float->(Float,Float)
calcularCirculo r = (2*p*r,p*r^2)
	where p = 3.1416
--Misma funcion con el let
calcularCirculo'::Float->(Float,Float)
calcularCirculo' r = let p=3.1416
	in (2*p*r,p*r^2)
	
--E)Implementar concat utilizando la definicion de listas por compresion (sin recursividad)
concatenar::[[Int]]->[Int]
concatenar lista=[elemento|sublistas<-lista,elemento<-sublistas]

--F)Dado un numero entero devuelva una lista por todos los factores de dicho numero, utilizando listas por compresion
divisores::Int->[Int]
divisores n = [x|x<-[1..n],n`mod`x==0]
--G) diga si un numero es primo, utiliza la funcion anterior
esPrimo::Int->Bool
esPrimo x = length(divisores x)==2

--H)Cuantos caracteres en mayuscula estan contenidos en una frase, utiliza listas por compresion
contarMayus::String->Int
contarMayus frase = length[letras|letras<-frase,isUpper letras]

--I)Dada una tupla de 3 elementos, con cada elemento siendo tupla de dos elementos de tipo String e Int -> devuelva el primer elemento de cada tupla
tuplas::((String,Int),(String,Int),(String,Int))->(String,String,String)
tuplas ((x1,y1),(x2,y2),(x3,y3))=(x1,x2,x3)

--J)Devuelve True si suma los 4 primeros elementos con un valor menor que 10 y False en caso contrario
primeros4::[Int]->Bool
primeros4 (a:b:c:d:ds) = ((a+b+c+d)<10)
primeros4 _ = False

--K)Dado un caracter, que representa un punto cardinal, devuelva su descripcion N=Norte
cardinales::Char->String
cardinales x
		|x=='N' = "Norte"
		|x=='S' = "Sur"
		|x=='E' = "Este"
		|x=='W' = "Oeste"
		|otherwise = "No es un punto cardinal"

--L)Dada una frase devuelva la primera y la ultima letra de la frase original
procesarFrase::String->String
procesarFrase frase = "La primera letra de la frase " ++ frase ++ "es" ++ [head frase] ++ "y la ultima es " ++ [last frase]

--M)Dado un numero entero devuelve mensajes indicando en que rango de valores se encuentra dicho numero (menor que 10, entre 10 y 20 o mayor 20) se deben utilizar definicones locales
rangoValores::Int->String
rangoValores x = entrada ++ if (x<10) then a else if ((x>=10)&&(x<=20)) then b else c
		where 
			entrada = "El valor de entrada"
			a = "menor que 10."
			b = "mayor o igual a 10 y menor o igual a 20."
			c = "mayor que 20."
			
--N)Dada una cadena de caracteres y un caracter, indique un numero de apariciones del caracter en la cadena. Sin recursividad y con listas de comprension
apariciones::String->Char->Int
apariciones frase letra=length[x|x<-frase,x==letra]

 