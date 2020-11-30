
module Hoja5 where

import Data.Char;
--1)
mayusMinus::IO()
mayusMinus = do
                putStrLn("Dame un nombre: ")
                nombre <- getLine
                let mayus = map toUpper nombre
                let minus = map toLower nombre
                putStrLn("En mayusculas: " ++ mayus)
                putStrLn("En minusculas: " ++ minus)
                
--2)
calcular::IO()
calcular = do
            putStrLn("Dame el primer operando: ")
            ope1 <- getLine
            putStrLn("Dame el segundo operando: ")
            ope2 <- getLine
            putStrLn("Quieres sumar 's' o restar 'r'")
            operacion <- getChar
            let ope1' = (read ope1) :: Int
            let ope2' = (read ope2) :: Int
            case operacion of 
                's' -> putStrLn(show(ope1' + ope2'))
                'r' -> putStrLn(show(ope1' - ope2'))
                _ -> do
                    putStrLn("No tengo esa operacion")
                    calcular

--3)Implementa una función que lea de un archivo de texto su contenido y lo reescribe en otro separando en dos 
-- líneas: una para las letras y otra para el resto de caracteres
letrasResto::IO()
letrasResto = do
                putStrLn("Fichero de entrada: ")
                fentrada <- getLine
                putStrLn("Fichero de salida: ")
                fsalida <- getLine
                cadena <- readFile fentrada
                let (letras, resto) = separar cadena
                writeFile fsalida (letras++"\n")
                appendFile fsalida (resto)
                putStrLn("Fichero escrito")
                
separar::String->(String,String)
separar cadena = foldl(\(letras,resto) e -> if(isAlpha e) then (letras++[e],resto) else if(not(isSpace e)) then (letras,resto++[e]) else (letras,resto))("","") cadena

--4)Implementa en Haskell una función que te vaya pidiendo líneas por pantalla y te vaya
-- mostrando su longitud hasta que introduzcas una de longitud 0 (vacía) 
longitudLinea::IO()
longitudLinea = do
                putStrLn("Dame una linea")
                linea <- getLine
                let longitud = length linea
                if (longitud /= 0) then do
                    putStrLn(show longitud)
                    longitudLinea
                    else
                    putStrLn("Adios")
                    
--5)Implementa un programa en Haskell que pida una frase y nos la devuelva con las palabras al reves sin cambiarlas de sitio
invertir::IO()
invertir = do
            putStrLn("Dame una linea \n")
            linea <- getLine
            let frase = unwords ( map reverse (words (linea)))
            if(length frase == 0)
                then putStrLn("Adios")
                    else do
                        putStrLn frase
                        invertir

--6)Diseña una función en Haskell que sea llamada con un número por pantalla y luego
-- lo intente adivinar (va diciendo mientras tanto si es mayor o menor) 
adivinar::Int->IO()
adivinar n = do
                putStrLn("Dame un numero: \n")
                inputNum <- getLine
                let num = (read inputNum) :: Int
                if (num == n) then
                    putStrLn("Has acertado")
                    else if (num < n) then do
                        putStrLn("Dame uno mayor")
                        adivinar n
                        else do
                            putStrLn("Dame uno menor")
                            adivinar n

--7)Los funtores son kk
--8)Funcion que lea dos archivos te los imprima por pantalla, los sume e imprima cada suma por separado
sumaArchivos::IO()
sumaArchivos = do
                putStrLn("Dame un archivo: ")
                fentrada1 <- getLine
                putStrLn("Dame otro archivo: ")
                fentrada2 <- getLine
                entrada1 <- readFile fentrada1
                entrada2 <- readFile fentrada2
                let numeros1 = [(read x) :: Int | x <- (words entrada1)]
                let numeros2 = [(read x) :: Int | x <- (words entrada2)]
                putStrLn(show numeros1)
                putStrLn(show numeros2 ++ "\n")
                sumaArchivosAux numeros1 numeros2 1
                
sumaArchivosAux::[Int]->[Int]->Int->IO()
sumaArchivosAux [] _ _ = putStrLn("")
sumaArchivosAux _ [] _ = putStrLn("")
sumaArchivosAux (x:xs) (y:ys) n = do
                putStrLn("Suma" ++ show n ++ ": " ++ show (x+y))
                sumaArchivosAux xs ys (n+1)
                
--9)Lee un archivo de texto, separe por frases y reescribe en dos archivos de manera alternativa NO SPLIT
--splitArchivos::IO()
--splitArchivos = do
                --putStrLn("Dame el archivo de lectura:")
                --fentrada <- getLine
                --putStrLn("Dame un archivo de salida:")
                --fsalida1 <- getLine
                --putStrLn("Dame otro archivo de salida:")
                --fsalida2 <- getLine
                --entrada <- readFile fentrada
                --let (pares, impares) = splitArchivosAux entrada
                --writeFile fsalida1 (filter even resultado)
                --writeFile fsalida2 (filter odd resultado)

--splitArchivosAux::String->(String,String)
--splitArchivosAux entrada = foldl(\(pares,impares) e -> 
--separar cadena = foldl(\(letras,resto) e -> if(isAlpha e) then (letras++[e],resto) else if(not(isSpace e)) then (letras,resto++[e]) else (letras,resto))("","") cadena
                

--9
divideEnArchivos::IO()
divideEnArchivos = do
    putStrLn("Dame un archivo")
    archivo1<-getLine
    putStrLn("Dame otro archivo")
    archivo2<-getLine
    putStrLn("Dame otro archivo")
    archivo3<-getLine
    texto<-readFile archivo1
    let (impares,pares) = textoParaArchivo (splitDePalo texto)
    writeFile archivo2 (impares)
    writeFile archivo3 (pares)
    
textoParaArchivo::[String]->(String,String)
textoParaArchivo l = textoParaArchivoAux l 1 ("","")

textoParaArchivoAux::[String]->Int->(String,String)->(String,String)
textoParaArchivoAux [] _ (impares,pares) = (impares,pares)
textoParaArchivoAux (x:xs) n (impares,pares) = if (even n) then textoParaArchivoAux xs (n+1) (impares,pares++""++x) else textoParaArchivoAux xs (n+1) (impares++""++x,pares)

splitDePalo::String->[String]
splitDePalo x = splitDePaloAux x [] []

splitDePaloAux::String->[String]->String->[String]
splitDePaloAux "" res acum = if (length acum == 0) then res else res++[acum]
splitDePaloAux (x:xs) res acum = if (x=='.') then splitDePaloAux xs (res++[acum++"."]) [] else splitDePaloAux xs res (acum++[x])       