{-# LANGUAGE DeriveGeneric #-}
module Parqueo where

{-Importacion de librerias-}
import Data.Aeson
import Data.Aeson.Encode.Pretty
import GHC.Generics
import qualified Data.ByteString.Lazy as BS


{--------------------------------------------------------------------------------------------------
Modelo: Parqueo
Versión: 1.0 (24 de noviembre del 2022)
Autor: Alex Sánchez Céspedes (https://github.com/alexuscr-27)
---------------------------------------------------------------------------------------------------
Descripción y propósito: 
    Representar el parqueo en el programa, almacenar y manipular los datos del programa.
Atributos:
    nombre: Nombre del parqueo.
    direccion: Direccion de ubicacion del parqueo.
    provincia: Provincia de ubicacion del parqueo.
    coordenada x: Coordenada x de ubicacion del parqueo.
    coordenada y: Coordenada y de ubicacion del parqueo.
--------------------------------------------------------------------------------------------------}
data Parqueo = Parqueo {
    nombre :: String,
    direccion :: String,
    provincia :: String,
    x :: Double,
    y :: Double
} deriving (Show, Generic)

instance ToJSON Parqueo
instance FromJSON Parqueo


{-
    Nombre: obtenerParqueos
    Descripcion: funcion que obtiene la informacion de los parqueos de un archivo externo
    Entradas: nombre del archivo
    Salidas: lista con la informacion de los parqueos
-}
obtenerParqueos :: String -> IO [Parqueo]
obtenerParqueos path = do
    contenido <- BS.readFile path
    let parqueos = decode contenido :: Maybe [Parqueo]
    case parqueos of
        Just parqueos -> return parqueos
        Nothing -> return []
        
{-
    Nombre: existeParqueo
    Descripcion: funcion que verifica si existe el nombre de un parqueo
    Entradas: nombre del archivo - nombre del parqueo
    Salidas: valor booleano
-}
existeParqueo :: String -> String -> IO Bool
existeParqueo path nombre = do
    parqueos <- obtenerParqueos path
    return (existeParqueo' parqueos nombre :: Bool)

{-
    Nombre: existeParqueo'
    Descripcion: funcion auxiliar de existeParqueo
    Entradas: nombre del archivo - nombre del parqueo
    Salidas: valor booleano
-}
existeParqueo' :: [Parqueo] -> String -> Bool
existeParqueo' [] nombre = False
existeParqueo' (x:xs) nombre = if (Parqueo.nombre x) == nombre then True else existeParqueo' xs nombre

{-
    Nombre: mostrarParqueo
    Descripcion: funcion que genera la cadena de informacion de los parqueos
    Entradas: un objeto de tipo Parqueo
    Salidas: un string
-}
mostrarParqueo :: Parqueo -> String
mostrarParqueo parqueo = "Nombre del Parqueo: " ++ (Parqueo.nombre parqueo) ++ "\n" ++
    "Direccion del Parqueo: " ++ (Parqueo.direccion parqueo) ++ "\n" ++
    "Provincia del Parqueo: " ++ (Parqueo.provincia parqueo) ++ "\n" ++
    "Coordenadas del Parqueo(x,y): (" ++ show (x parqueo) ++ ","++ show (y parqueo) ++ ")"

{-
    Nombre: mostrarParqueos
    Descripcion: funcion que imprime los datos de los parqueos
    Entradas: la ubicacion del archivo
    Salidas: sin salidas
-}
mostrarParqueos :: String -> IO ()
mostrarParqueos path = do
    parqueos <- obtenerParqueos path
    putStrLn (mostrarParqueos' parqueos)

{-
    Nombre: mostrarParqueos'
    Descripcion: funcion auxiliar de mostrarParqueos
    Entradas: lista con la informacion de todos los parqueos
    Salidas: un string
-}
mostrarParqueos' :: [Parqueo] -> String
mostrarParqueos' [] = ""
mostrarParqueos' (x:xs) = (mostrarParqueo x) ++ "\n--------------------------------------------------------------------------------------------------\n" ++(mostrarParqueos' xs)

{-
    Nombre: mostrarParqueosPorProvincia
    Descripcion: funcion que imprime los parqueos apartir de una provincia
    Entradas: ubicacion del archivo - nombre de la provincia
    Salidas: un string
-}
mostrarParqueosPorProvincia :: String -> String -> IO ()
mostrarParqueosPorProvincia path provincia = do
    parqueos <- obtenerParqueos path
    putStrLn (mostrarParqueosPorProvincia' parqueos provincia)

{-
    Nombre: mostrarParqueosPorProvincia'
    Descripcion: funcion auxiliar de mostrarParqueosPorProvincia
    Entradas: lista con la informacion de los parqueos - nombre de la provincia
    Salidas: un string
-}
mostrarParqueosPorProvincia' :: [Parqueo] -> String -> String
mostrarParqueosPorProvincia' [] provincia = ""
mostrarParqueosPorProvincia' (x:xs) provincia = 
    if (Parqueo.provincia x) == provincia then (mostrarParqueo x) ++ "\n-----------------------------------------------------\n" ++
        (mostrarParqueosPorProvincia' xs provincia) 
    else mostrarParqueosPorProvincia' xs provincia