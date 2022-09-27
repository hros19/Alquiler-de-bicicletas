{-# LANGUAGE DeriveGeneric #-}

module Comercio where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import GHC.Generics
import qualified Data.ByteString.Lazy as BS

-- Variables globales

path_comercio :: FilePath
path_comercio = "./src/data/comercio.json"

{--------------------------------------------------------------------------------------------------
Modelo: Comercio
Versión: 1.0 (24 de setiembre del 2022)
Autor: Hansol Antay Rostrán (https://github.com/hros19)
---------------------------------------------------------------------------------------------------
Descripción y propósito: 
    Representará al comercio del sistema. Su objetivo es poder almacenar y manipular los datos 
    del comercio.
Atributos:
    nombre: Nombre del comercio.
    web: Página web del comercio.
    contacto: Contacto del comercio (Correo o teléfono).
    tarifaColonesKmPedal: Tarifa en colones por kilómetro de pedal del comercio.
    tarifaColonesKmElectrico: Tarifa en colones por kilómetro de eléctrico del comercio.
--------------------------------------------------------------------------------------------------}
data Comercio = Comercio {
    nombre :: String,
    web :: String,
    contacto :: String,
    tarifaColonesKmPedal :: Double,
    tarifaColonesKmElectrico :: Double
} deriving (Show, Generic)

instance ToJSON Comercio
instance FromJSON Comercio

{-
    Función: guardarComercio
    Descripción: Guarda un comercio en el archivo de comercio.
    Parámetros:
        comercio: Comercio a guardar.
    Retorno: --
    Restricciones: --
-}
guardarComercio :: Comercio -> IO ()
guardarComercio comercio = do
    BS.writeFile path_comercio (encodePretty comercio)

{-
    Función: obtenerComercio
    Descripción: Obtiene el comercio del archivo de comercio.
    Objetivo: Obtener el comercio del archivo de comercio, para mosttrar los datos del comercio.
    Parámetros: --
    Retorno: objeto Comercio.
    Restricciones: --
-}
obtenerComercio :: IO Comercio
obtenerComercio = do
    contenido <- BS.readFile path_comercio
    let comercio = decode contenido :: Maybe Comercio
    case comercio of
        Just comercio -> return comercio
        Nothing -> return (Comercio "" "" "" 0 0)

{-
    Función: mostrarComercio
    Descripción: muestra todos los datos del comercio.
    Objetivo: mostrar todos los datos del comercio al usuario.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
mostrarComercio :: IO ()
mostrarComercio = do
    comercio <- obtenerComercio
    putStrLn ("Nombre: " ++ nombre comercio)
    putStrLn ("Web: " ++ web comercio)
    putStrLn ("Contacto: " ++ contacto comercio)
    putStrLn ("Tarifa por kilómetro de pedal: " ++ show (tarifaColonesKmPedal comercio))
    putStrLn ("Tarifa por kilómetro de eléctrico: " ++ show (tarifaColonesKmElectrico comercio))

{-
    Función: mostrarComercioSinTarifas
    Descripción: muestra todos los datos del comercio excepto las tarifas.
    Objetivo: mostrar todos los datos del comercio al usuario excepto las tarifas (en facturas).
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
mostrarComercioSinTarifas :: IO ()
mostrarComercioSinTarifas = do
    comercio <- obtenerComercio
    putStrLn ("Nombre: " ++ nombre comercio)
    putStrLn ("Web: " ++ web comercio)
    putStrLn ("Contacto: " ++ contacto comercio)

{-
    Función: obtenerTarifaSegúnTipo
    Descripción: permite obtener la tarifa según el tipo de bicicleta.
    Parámetros:
    - tipoBicicleta: Tipo de bicicleta.
    Retorno: Tarifa según el tipo de bicicleta.
    Restricciones: --
-}
obtenerTarifaSegunTipo :: String -> IO Double
obtenerTarifaSegunTipo tipo = do
    comercio <- obtenerComercio
    case tipo of
        "TR" -> return (tarifaColonesKmPedal comercio)
        "AE" -> return (tarifaColonesKmElectrico comercio)
        _ -> return 0