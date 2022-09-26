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

guardarComercio :: Comercio -> IO ()
guardarComercio comercio = do
    BS.writeFile path_comercio (encodePretty comercio)

obtenerComercio :: IO Comercio
obtenerComercio = do
    contenido <- BS.readFile path_comercio
    let comercio = decode contenido :: Maybe Comercio
    case comercio of
        Just comercio -> return comercio
        Nothing -> return (Comercio "" "" "" 0 0)

mostrarComercio :: IO ()
mostrarComercio = do
    comercio <- obtenerComercio
    putStrLn ("Nombre: " ++ nombre comercio)
    putStrLn ("Web: " ++ web comercio)
    putStrLn ("Contacto: " ++ contacto comercio)
    putStrLn ("Tarifa por kilómetro de pedal: " ++ show (tarifaColonesKmPedal comercio))
    putStrLn ("Tarifa por kilómetro de eléctrico: " ++ show (tarifaColonesKmElectrico comercio))

mostrarComercioSinTarifas :: IO ()
mostrarComercioSinTarifas = do
    comercio <- obtenerComercio
    putStrLn ("Nombre: " ++ nombre comercio)
    putStrLn ("Web: " ++ web comercio)
    putStrLn ("Contacto: " ++ contacto comercio)

obtenerTarifaSegunTipo :: String -> IO Double
obtenerTarifaSegunTipo tipo = do
    comercio <- obtenerComercio
    case tipo of
        "TR" -> return (tarifaColonesKmPedal comercio)
        "AE" -> return (tarifaColonesKmElectrico comercio)
        _ -> return 0