
module Hoja3 where
--A
--1
--Carlos
data RacionalData = Rac {num :: Int, den :: Int} deriving Show

equivalentesData :: [RacionalData] -> RacionalData -> [RacionalData]
equivalentesData [] _ = []
equivalentesData lista r1 = foldr (\r2 acum -> if (num r1 * den r2) == (num r2 * den r1) then r2 : acum else acum) [] lista
--2
type RacionalType = (Int,Int)

equivalentesType::[RacionalType]->RacionalType->[RacionalType]
equivalentesType [] _ = []
equivalentesType lista (num1,den1)=foldr(\(num2,den2) acum ->if(num1*den2)==(num2*den1)then(num2,den2):acum else acum)[] lista

--B
data Coordenadas = Coord {x::Float, y::Float} deriving Show
data PuntoCardinal = Norte|Sur|Este|Oeste
--1
mover::PuntoCardinal->Coordenadas->Coordenadas
mover Norte (Coord x y) = (Coord x(y+1))
mover Sur (Coord x y) = (Coord x(y-1))
mover Este (Coord x y) = (Coord (x+1)y)
mover Oeste (Coord x y) = (Coord (x-1)y)
--2
sur::Coordenadas->Coordenadas->Coordenadas
sur p1 p2 = if (y p1)<(y p2) then p1 else p2
--3
distancia::Coordenadas->Coordenadas->Float
distancia p1 p2 = sqrt (((x p2) - (x p1)) ^ 2 + ((y p2) - (y p1)) ^ 2)
--4
camino::Coordenadas->[PuntoCardinal]->[Coordenadas]
camino_ [] = []
camino p (x:xs) = (mover x p) : camino(mover x p) xs
--C
data DiaSemana= Lunes|Martes|Miercoles|Jueves|Viernes|Sabado|Domingo

laborable::DiaSemana->Bool
laborable Sabado = False
laborable Domingo = False
laborable _ = True

--D)Definir temperaturas Far y Cel, implementar un conversor, definir tipo de datos de ordenes ON y OFF, definir si se debe encender o apagar el aire acondicionado

data UnidadTem = Cel | Far deriving Show
data Temperatura = Temp (Float,UnidadTem) deriving Show
--2
instance Eq UnidadTem where
  Cel == Cel = True
  Far == Far = True
  _ == _ = False

convert:: Temperatura->Temperatura
convert (Temp(grados,unidad))
  |unidad== Far = Temp (((grados-32)*5/9),Cel)
  |otherwise = Temp(((grados)*9/5+32),Far)

--d1
data Temperatura = C Float | F Float

--d2
dConvert:: Temperatura->Temperatura
dConvert (C x) = F (x*(9 / 5)+32)
dConvert (F x) = C ((x-32)*(5 / 9))

convert:: Temperatura->Temperatura
convert (Temp(grados,unidad))
	|unidad== Far = Temp (((grados-32)*5/9),Cel)
	|otherwise = Temp(((grados)*9/5+32),Far)

--3
data AccionAireAcc = On|Off deriving Show

--4
action::Temperatura->AccionAireAcc
action (Temp(grados,unidad))
	|unidad==Cel = if grados > 28 then On else Off
	|otherwise = action(convert (Temp (grados,unidad)))

--E
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
