{-# LANGUAGE DeriveGeneric #-}

module Parqueo where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import GHC.Generics
import qualified Data.ByteString.Lazy as BS

-- de momento -- 
path_parqueo :: FilePath
path_parqueo = "./src/data/parqueo.json"

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

obtenerParqueos :: String -> IO [Parqueo]
obtenerParqueos path = do
    contenido <- BS.readFile path
    let parqueos = decode contenido :: Maybe [Parqueo]
    case parqueos of
        Just parqueos -> return parqueos
        Nothing -> return []

existeParqueo :: String -> String -> IO Bool
existeParqueo path nombre = do
    parqueos <- obtenerParqueos path
    return (existeParqueo' parqueos nombre :: Bool)

existeParqueo' :: [Parqueo] -> String -> Bool
existeParqueo' [] nombre = False
existeParqueo' (x:xs) nombre = if (Parqueo.nombre x) == nombre then True else existeParqueo' xs nombre


mostrarParqueo :: Parqueo -> String
mostrarParqueo parqueo = "Nombre del Parqueo: " ++ (Parqueo.nombre parqueo) ++ "\n" ++
                             "Direccion del Parqueo: " ++ (Parqueo.direccion parqueo) ++ "\n" ++
                             "Provincia del Parqueo: " ++ (Parqueo.provincia parqueo) ++ "\n" ++
                             "Coordenadas del Parqueo(x,y): (" ++ show (x parqueo) ++ ","++ show (y parqueo) ++ ")"

mostrarParqueos :: String -> IO ()
mostrarParqueos path = do
    parqueos <- obtenerParqueos path
    putStrLn (mostrarParqueos' parqueos)

mostrarParqueos' :: [Parqueo] -> String
mostrarParqueos' [] = ""
mostrarParqueos' (x:xs) = (mostrarParqueo x) ++ 
                            "\n-----------------------------------------------------\n" ++
                            (mostrarParqueos' xs)