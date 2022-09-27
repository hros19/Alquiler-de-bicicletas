{-# LANGUAGE DeriveGeneric #-}

module Recibo where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import GHC.Generics
import qualified Data.ByteString.Lazy as BS

path_recibos :: FilePath
path_recibos = "./src/data/recibos.json"

{--------------------------------------------------------------------------------------------------
Modelo: Recibo
Versión: 1.0 (27 de setiembre del 2022)
Autor: Hansol Antay Rostrán (https://github.com/hros19)
---------------------------------------------------------------------------------------------------
Descripción y propósito: 
    Representará el registro de las facturas canceladas por los clientes.
Atributos:
    idFactura: Identificador de la factura.
    cedulaCliente: Cédula del cliente que canceló la factura.
    nomreCliente: Nombre del cliente que canceló la factura.
    parqueoSalida: Parqueo de salida de la bicicleta.
    parqueoLlegada: Parqueo de llegada de la bicicleta.
    codigoBici: Código de la bicicleta alquilada.
    tipoBici: Tipo de bicicleta alquilada. (TR-Tradicional, AE-Asistencia eléctrica)
    tarifaPorKilometro: Tarifa por kilómetro de la bicicleta alquilada.
    distanciaRecorridaEnKm: Distancia recorrida por la bicicleta alquilada.
    costoTotal: Costo total de la factura.
--------------------------------------------------------------------------------------------------}

data Recibo = Recibo {
    idFactura :: Int,
    cedulaCliente :: String,
    parqueoSalida :: String,
    parqueoLlegada :: String,
    codigoBici :: String,
    tipoBici :: String,
    tarifaPorKilometro :: Double,
    distanciaRecorridaEnKm :: Double,
    costoTotal :: Double
} deriving (Show, Generic)

instance ToJSON Recibo
instance FromJSON Recibo

{-
    Función: obtenerRecibos
    Descripción: Obtiene la lista de recibos del archivo JSON.
    Parámetros: --
    Retorno: Lista de recibos.
    Restricciones: --
-}
obtenerRecibos :: IO [Recibo]
obtenerRecibos = do
    contenido <- BS.readFile path_recibos
    let recibos = decode contenido :: Maybe [Recibo]
    case recibos of
        Nothing -> return []
        Just recibos -> return recibos

{-
    Función: agregarRecibo
    Descripción: Agrega un recibo a la lista de recibos.
    Parámetros: recibo: Recibo a agregar.
    Retorno: --
    Restricciones: --
-}
agregarRecibo :: Recibo -> IO ()
agregarRecibo recibo = do
    recibos <- obtenerRecibos
    let recibosActualizados = recibos ++ [recibo]
    let contenido = encodePretty recibosActualizados
    BS.writeFile path_recibos contenido
