
module Hoja3 where
--A)Dada una lista de racionales, dos int (mun y dem) y un numero racional, devuelva otra con todos los racionales equivalentes.
--1)Empleando data
data RacionalData = Rac {num :: Int, den :: Int} deriving Show

equivalentesData :: [RacionalData] -> RacionalData -> [RacionalData]
equivalentesData [] _ = []
equivalentesData lista r1 = foldr (\r2 acum -> if (num r1 * den r2) == (num r2 * den r1) then r2 : acum else acum) [] lista
--2)Empleando type
type RacionalType = (Int,Int)

equivalentesType::[RacionalType]->RacionalType->[RacionalType]
equivalentesType [] _ = []
equivalentesType lista (num1,den1)=foldr(\(num2,den2) acum ->if(num1*den2)==(num2*den1)then(num2,den2):acum else acum)[] lista

--B)Funcion de coordeandas, definir tipo
data Coordenadas = Coord {x::Float, y::Float} deriving Show
data PuntoCardinal = Norte|Sur|Este|Oeste

--1)Direccion mas coordenadas realiza el movimiento
mover::PuntoCardinal->Coordenadas->Coordenadas
mover Norte (Coord x y) = (Coord x(y+1))
mover Sur (Coord x y) = (Coord x(y-1))
mover Este (Coord x y) = (Coord (x+1)y)
mover Oeste (Coord x y) = (Coord (x-1)y)

--2)Entre 2 coordenadas indicar la mas al sur
sur::Coordenadas->Coordenadas->Coordenadas
sur p1 p2 = if (y p1)<(y p2) then p1 else p2

--3)Distancia entre dos puntos
distancia::Coordenadas->Coordenadas->Float
distancia p1 p2 = sqrt (((x p2) - (x p1)) ^ 2 + ((y p2) - (y p1)) ^ 2)

--4)Dado un punto y una lista de direcciones retorna el camino que se recorre
camino::Coordenadas->[PuntoCardinal]->[Coordenadas]
camino_ [] = []
camino p (x:xs) = (mover x p) : camino(mover x p) xs

--C)Dado un dia de la semana indique si es o no laborable
data DiaSemana= Lunes|Martes|Miercoles|Jueves|Viernes|Sabado|Domingo

laborable::DiaSemana->Bool
laborable Sabado = False
laborable Domingo = False
laborable _ = True

--D)Definir temperaturas Far y Cel, implementar un conversor, definir tipo de datos de ordenes ON y OFF, definir si se debe encender o apagar el aire acondicionado
--1)Definir el tipo de dato
data UnidadTem = Cel | Far deriving Show
data Temperatura = Temp (Float,UnidadTem) deriving Show

--2)Definir una funcion de conversion entre temperaturas
instance Eq UnidadTem where
  Cel == Cel = True
  Far == Far = True
  _ == _ = False

convert:: Temperatura->Temperatura
convert (Temp(grados,unidad))
  |unidad== Far = Temp (((grados-32)*5/9),Cel)
  |otherwise = Temp(((grados)*9/5+32),Far)

--3)Tipo de datos para representar acciones
data AccionAireAcc = On|Off deriving Show

--4)Definir action que dad una temperatura determine la accion a realizar
action::Temperatura->AccionAireAcc
action (Temp(grados,unidad))
	|unidad==Cel = if grados > 28 then On else Off
	|otherwise = action(convert (Temp (grados,unidad)))

--E)Definir el tipo de moneda para representar euros y dolares USA, asi como la funcion que convierta las divisas (1.14)
data UnidadMonetaria = Euro|Dolar deriving Show
data Moneda = Moneda(Float,UnidadMonetaria) deriving Show

instance Eq UnidadMonetaria where
	Euro == Euro = True
	Dolar == Dolar = True
	_ == _ = False

convertMonedas::Moneda->Moneda
convertMonedas (Moneda(cantidad,unidad))
	|unidad == Euro= Moneda(cantidad *1.14,Dolar)
	|otherwise = Moneda(cantidad/1.14,Euro)

--F)Dada el siguiente tipo
data Expr = Valor Integer
			|Expr :-: Expr
			|Expr :+: Expr
			|Expr :*: Expr deriving Show

calculavalor:Expr->Int
calculaValor (Valor x) = x
calculaValor (a:*:b)=calculaValor a)*(calculaValor b)