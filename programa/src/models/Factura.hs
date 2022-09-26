{-# LANGUAGE DeriveGeneric #-}

module Factura where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import GHC.Generics
import qualified Data.ByteString.Lazy as BS

-- Variables globales

path_facturas :: FilePath
path_facturas = "./src/data/facturas.json"

{--------------------------------------------------------------------------------------------------
Modelo: Factura
Versión: 1.0 (25 de setiembre del 2022)
Autor: Hansol Antay Rostrán (https://github.com/hros19)
---------------------------------------------------------------------------------------------------
Descripción y propósito: 
    Representará una factura en la tienda de bicicletas.
Atributos:
    idFactura: Identificador de la factura.
    cedulaCliente: Cédula del cliente que realizó el alquiler.
    parqueoSalida: Parqueo de salida de la bicicleta.
    parqueoLlegada: Parqueo de llegada de la bicicleta.
    codigoBici: Código de la bicicleta alquilada.
    estado: Estado de la factura.
--------------------------------------------------------------------------------------------------}

data Factura = Factura {
    idFactura :: Int,
    cedulaCliente :: String,
    parqueoSalida :: String,
    parqueoLlegada :: String,
    codigoBici :: String,
    estado :: String
} deriving (Show, Generic)

instance FromJSON Factura
instance ToJSON Factura

obtenerFacturas :: IO [Factura]
obtenerFacturas = do
    contenido <- BS.readFile path_facturas
    let facturas = decode contenido :: Maybe [Factura]
    case facturas of
        Nothing -> return []
        Just facturas -> return facturas

mostrarFactura :: Factura -> String 
mostrarFactura factura = "Factura #" ++ show (idFactura factura) ++ "\n" ++
                         "Cédula del cliente: " ++ cedulaCliente factura ++ "\n" ++
                         "Parqueo de salida: " ++ parqueoSalida factura ++ "\n" ++
                         "Parqueo de llegada: " ++ parqueoLlegada factura ++ "\n" ++
                         "Código de la bicicleta: " ++ codigoBici factura ++ "\n" ++
                         "Estado: " ++ estado factura ++ "\n" ++
                         "--------------------------------------------\n"

mostrarFacturas :: IO ()
mostrarFacturas = do
    facturas <- obtenerFacturas
    putStrLn (mostrarFacturas' facturas)

mostrarFacturas' :: [Factura] -> String
mostrarFacturas' [] = ""
mostrarFacturas' (x:xs) = mostrarFactura x ++ mostrarFacturas' xs

agregarFactura :: Factura -> IO ()
agregarFactura factura = do
    facturas <- obtenerFacturas
    let facturas' = facturas ++ [factura]
    BS.writeFile path_facturas (encodePretty facturas')

actualizarFactura :: Factura -> IO ()
actualizarFactura factura = do
    facturas <- obtenerFacturas
    let facturas' = actualizarFactura' factura facturas
    BS.writeFile path_facturas (encodePretty facturas')

actualizarFactura' :: Factura -> [Factura] -> [Factura]
actualizarFactura' factura [] = []
actualizarFactura' factura (x:xs)
    | idFactura factura == idFactura x = factura : xs
    | otherwise = x : actualizarFactura' factura xs

existeFactura :: Int -> IO Bool
existeFactura idFactura = do
    facturas <- obtenerFacturas
    return (existeFactura' idFactura facturas)

existeFactura' :: Int -> [Factura] -> Bool
existeFactura' idF [] = False
existeFactura' idF (x:xs)
    | idF == (idFactura x) = True
    | otherwise = existeFactura' idF xs


