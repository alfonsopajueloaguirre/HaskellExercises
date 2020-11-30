
module Hoja2 where

--A)Eliminar de una lista los multiplos de x
--Por lista por compresion
cribar::[Int]->Int->[Int]
cribar lista multi = [e|e<-lista, e `mod` multi /= 0]

--Con recursividad no final
cribar2::[Int]->Int->[Int]
cribar2 [] _= []
cribar2 (x:xs) multi = if x`mod`multi == 0 then x:(cribar2 xs multi) else cribar2 xs multi

--Con recursividad final o de cola
cribar3::[Int]->Int->[Int]
cribar3 [] _ = []
cribar3 (x:xs) multi = cribarAux (x:xs) multi []

cribarAux::[Int]->Int->[Int]->[Int]
cribarAux [] _ acum = acum
cribarAux (x:xs) multi acum = if(x`mod`multi/=0) then (cribarAux xs multi acum++[x])else (cribarAux xs multi acum)

--B) Funcion doble con expresion lambda
doble::Int->Int
doble x = x+x

doble'::Int->Int
doble' = (\x->x+x)

--C)Dada una lista de enteros obtenga un resultado int = suma del doble de cada uno de los elementos
--Con recursividad no final
sumaDobles::[Int]->Int
sumaDobles [] = 0
sumaDobles (x:xs) =  x*2 + sumaDobles xs

--Con recursividad final 
sumaDobles2::[Int]->Int
sumaDobles2 [] = 0
sumaDobles2 (x:xs) = sumaDoblesAux (x:xs) 0

sumaDoblesAux::[Int]->Int->Int
sumaDoblesAux [] acum = acum
sumaDoblesAux (x:xs) acum = sumaDoblesAux xs acum + x*2

--Orden Superior
sumaDobles3::[Int]->Int
sumaDobles3 lista = foldl(\acum e ->acum + e*2) 0 lista

--D)Funcion que sume los cuadrados de los numeros pares contenidos en una lista
--Usar map y filter
sumaCuadrados::[Int]->Int
sumaCuadrados [] = 0
sumaCuadrados lista = sumaCuadradosAux(map (^2) (filter even lista))

sumaCuadradosAux::[Int]->Int
sumaCuadradosAux (x:xs) = x + sumaCuadradosAux xs
--Lista por compresion
sumaCuadrados'::[Int]->Int
sumaCuadrados' lista = sumaCuadradosAux'[e|e<-lista, even e]

sumaCuadradosAux'::[Int] -> Int
sumaCuadradosAux' lista = foldr(\e acum ->acum + e ) 0 [x^2|x<-lista,even x]

--E)Devolver tuplas de elementos sin repetir junto con la posicion que aparecen
primeraAparicion:: [Int]->[(Int,Int)]
primeraAparicion l = pAux l 1 []

existe::[Int]->Int->Bool
existe [] _ = False
existe (x:xs) c = (x==c) || existe xs c

pAux::[Int]->Int->[Int]->[(Int,Int)]
pAux []_ _ = []
pAux (x:xs) contador aparecidos = if (existe aparecidos x) then (pAux xs (contador+1) aparecidos)
                                    else (x,contador):(pAux xs (contador+1)(x:aparecidos))
                                    
--F)Calcula el numero de secuencias de ceros que hay en una lista
ceros::[Int]->Int
ceros [] = 0
ceros [x] = if x == 0 then 1 else 0
ceros (x:y:ys) = if (x==0)&&(y/=0) then 1 + ceros ys else ceros(y:ys)

--G)Recibe una lista y devuelve dos listas repetidos y sin repetir
repetidos::[Int]->[[Int]]
repetidos lista = foldr(\e [repetidos,unicos]->if existe repetidos e then [repetidos,unicos]
											else if existe unicos e then [e:repetidos,eliminar unicos e]
											else [repetidos,e:unicos])
					   [[],[]] lista

eliminar::[Int]->Int->[Int]
eliminar [] _ =[]
eliminar lista elemento = [x|x<-lista,x/=elemento]

--H)Dada una lista devolver otra lista con los n elementos de mayor valor
nMayores::[Int]->Int->[Int]
nMayores [] _ = []
nMayores _ 0 = []
nMayores lista n = take n (mergeSort lista)

mergeSort::[Int]->[Int]
mergeSort []=[]
mergeSort[x]=[x]
mergeSort l = fusion(mergeSort a, mergeSort b)
    where
     (a,b) = partir (l,[],[])
    
partir::([Int],[Int],[Int])->([Int],[Int])
partir([],l1,l2)=(l1,l2)
partir([x],l1,l2)=(x:l1,l2)
partir(a:b:xs,l1,l2)= partir (xs,a:l1,b:l2)

fusion::([Int],[Int])->[Int]
fusion (l,[])=l
fusion ([],l)=l
fusion (x:xs,y:ys)= if(x<y)then x:fusion(xs,y:ys)else y:fusion(x:xs,ys)

--I)Dada dos listas nos tiene que decir si la primera esta contenida en la segunda
incluye::[Int]->[Int]->Bool
incluye [] _ = True
incluye lista [] = False
incluye lista lista2 = incluyeAux lista lista lista2

incluyeAux :: [Int] -> [Int] -> [Int] -> Bool
incluyeAux [] [] _ = True
incluyeAux _ _ [] = False
incluyeAux lista (x:xs) (y:ys) = if (x == y) then True && incluyeAux lista xs ys else incluyeAux lista lista ys

--J)Dada una lista ordenarla de mayor a menor con algoritmo de insercion
ordenar:: [Int]->[Int]
ordenar [] = []
ordenar (x:xs) = ordenarAux x (ordenar xs)

ordenarAux::Int->[Int]->[Int]
ordenarAux x [] = [x]
ordenarAux x (y:ys) = if (x<y) then (x:y:ys) else y:ordenarAux x ys

--K)Recibe dos listas y coges un elementos de la primera y dos de la segunda, creando una lista final de ternas
mezclarTernas::[a]->[b]->[(a,b,b)]
mezclarTernas [] _ = []
mezclarTernas _ [] = []
mezclarTernas (x:xs) (y:z:zs)= (x,y,z) : mezclarTernas xs zs

--L)Funcion polimorfica dado un elemento y una lista añada el elemento al final de la lista
alFinal::a->[a]->[a]
alFinal a [] = [a]
alfinal elemento lista = lista ++[elemento]								
--M)Implementar la funcion zipWith reibe una funcion y dos listas y une ambas aplicando la funcion 
zipWith1::(a->b->c)->[a]->[b]->[c]
zipWith1 f l1 l2 = foldl(\acum (x,y)->acum++[f x y])[](duplas l1 l2)

duplas::[a]->[b]->[(a,b)]
duplas [] _ = []
duplas _ [] = []
duplas (x:xs)(y:ys) = (x,y):(duplas xs ys)

--N)Funcion polimorfica capaz de invertir los elementos de una lista
--No final
rever::[a]->[a]
rever [] = []
rever (x:xs) = rever xs ++ [x]

--Final
rever2::[a]->[a]
rever2 [] = []
rever2 lista = rever2Aux lista []

rever2Aux::[a]->[a]->[a]
rever2Aux [] acum = acum
rever2Aux (x:xs) acum = rever2Aux xs [x]++acum

--Foldr
rever3::[a]->[a]
rever3 [] = []
rever3 lista = foldr (\elemento acum -> acum++[elemento])[] lista

--O)Funcion polimorfica que invierta los elementos de una lista de listas
reverLista::[[a]]->[[a]]
reverLista [] = []
reverLista lista = reverAux lista []


reverAux::[[a]]->[[a]]->[[a]]
reverAux [] acum = acum
reverAux (x:xs) acum = reverAux xs [reverse x] ++ acum

--P)Implementar la funcion flip, recibe una funcion y devuelve otra identica a la original pero con los parametros intercambiados
flip1::(a->a->b)->a->a->b
flip1 function x y = function y x

--Q)Funcion polimorfica map, recibe una funcion y una lista y devuelve la resultante al aplicar la f a cada ex
map1::(a->b)->[a]->[b]
map1 _ [] = [] 
map1 function (x:xs) = [function x] ++ map1 function xs











