{-# LANGUAGE DeriveGeneric #-}

module Factura where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import GHC.Generics
import qualified Data.ByteString.Lazy as BS

import Parqueo
import Bicicleta
import Comercio
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

{-
    Función: obtenerFacturas
    Descripción: Obtiene todas las facturas registradas en el sistema.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
obtenerFacturas :: IO [Factura]
obtenerFacturas = do
    contenido <- BS.readFile path_facturas
    let facturas = decode contenido :: Maybe [Factura]
    case facturas of
        Nothing -> return []
        Just facturas -> return facturas

{-
    Función: mostrarFactura
    Descripción: Devuelve una factura en formato String.
    Objetivo: permitir mostrar una factura en la interfaz de consola.
    Parámetros:
        factura: Factura a mostrar.
    Retorno: un String con la factura.
    Restricciones: --
-}
mostrarFactura :: Factura -> String 
mostrarFactura factura = "Factura #" ++ show (idFactura factura) ++ "\n" ++
                         "Cédula del cliente: " ++ cedulaCliente factura ++ "\n" ++
                         "Parqueo de salida: " ++ parqueoSalida factura ++ "\n" ++
                         "Parqueo de llegada: " ++ parqueoLlegada factura ++ "\n" ++
                         "Código de la bicicleta: " ++ codigoBici factura ++ "\n" ++
                         "Estado: " ++ estado factura ++ "\n" ++
                         "--------------------------------------------\n"

{-
    Función: mostrarFacturas
    Descripción: muestra todas las facturas registradas en el sistema.
    Objetivo: permitir mostrar todas las facturas en la interfaz de consola.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
mostrarFacturas :: IO ()
mostrarFacturas = do
    facturas <- obtenerFacturas
    putStrLn (mostrarFacturas' facturas)

{-
    Función: mostrarFacturas'
    Descripción: función auxiliar de mostrarFacturas.
    Objetivo: va recorriendo la lista de facturas y las va mostrando.
    Parámetros:
        facturas: lista de facturas a mostrar.
    Retorno: un String con todas las facturas.
    Restricciones: --
-}
mostrarFacturas' :: [Factura] -> String
mostrarFacturas' [] = ""
mostrarFacturas' (x:xs) = mostrarFactura x ++ mostrarFacturas' xs

{-
    Función: agregarFactura
    Descripción: agrega una factura al sistema.
    Objetivo: permitir agregar una factura al sistema.
    Parámetros:
        factura: factura a agregar.
    Retorno: --
    Restricciones: --
-}
agregarFactura :: Factura -> IO ()
agregarFactura factura = do
    facturas <- obtenerFacturas
    let facturas' = facturas ++ [factura]
    BS.writeFile path_facturas (encodePretty facturas')

{-
    Función: actualizarFactura
    Descripción: actualiza una factura en el sistema.
    Objetivo: permitir actualizar una factura en el sistema (dentro del archivo)
    Parámetros:
        factura: factura a actualizar.
    Retorno: --
    Restricciones: --
-}
actualizarFactura :: Factura -> IO ()
actualizarFactura factura = do
    facturas <- obtenerFacturas
    let facturas' = actualizarFactura' factura facturas
    BS.writeFile path_facturas (encodePretty facturas')

{-
    Función: actualizarFactura'
    Descripción: Función auxiliar de actualizarFactura.
    Objetivo: va recorriendo la lista de facturas y actualiza la factura que se le pasa como parámetro.
    Parámetros:
        factura: factura a actualizar.
        facturas: lista de facturas.
    Retorno: la lista de facturas actualizada.
    Restricciones: --
-}
actualizarFactura' :: Factura -> [Factura] -> [Factura]
actualizarFactura' factura [] = []
actualizarFactura' factura (x:xs)
    | idFactura factura == idFactura x = factura : xs
    | otherwise = x : actualizarFactura' factura xs

{-
    Función: obtenerFacturasDeBicicleta
    Descripción: obtiene todas las facturas de una bicicleta.
    Objetivo: permitir que se consulte las facturas de una bicicleta, con su código.
    Parámetros:
        - codigo: código de la bicicleta.
    Retorno: el listado de facturas de la bicicleta.
    Restricciones: --
-}
obtenerFacturasDeBicicleta :: String -> IO [Factura]
obtenerFacturasDeBicicleta codigoBici = do
    facturas <- obtenerFacturas
    return (obtenerFacturasDeBicicleta' codigoBici facturas)

{-
    Función: obtenerFacturasDeBicicleta'
    Descripción: función auxiliar de obtenerFacturasDeBicicleta.
    Objetivo: va recorriendo la lista de facturas y va agregando las facturas que corresponden a la bicicleta.
    Parámetros:
        - codigo: código de la bicicleta.
        - facturas: lista de facturas.
    Retorno: el listado de facturas de la bicicleta.
    Restricciones: --
-}
obtenerFacturasDeBicicleta' :: String -> [Factura] -> [Factura]
obtenerFacturasDeBicicleta' codigoB [] = []
obtenerFacturasDeBicicleta' codigoB (x:xs)
    | (codigoB == codigoBici x && "facturado" == estado x) = x : obtenerFacturasDeBicicleta' codigoB xs
    | otherwise = obtenerFacturasDeBicicleta' codigoB xs

{-
    Función: obtenerDistanciaEntreParqueos
    Descripción: obtiene la distancia entre dos parqueos.
    Objetivo: permitir calcular la distancia entre dos parqueos.
    Parámetros:
        - parqueoSalida: parqueo de salida.
        - parqueoLlegada: parqueo de llegada.
    Retorno: la distancia entre los dos parqueos.
    Restricciones: --
-}
obtenerDistanciaEntreParqueos :: String -> String -> IO Double
obtenerDistanciaEntreParqueos parqueoSalida parqueoLlegada = do
    parqueoSal <- Parqueo.obtenerParqueo "./src/data/parqueo.json" parqueoSalida
    parqueoLleg <- Parqueo.obtenerParqueo "./src/data/parqueo.json" parqueoLlegada
    return (Parqueo.distanciaEntreDosParqueos parqueoSal parqueoLleg)

{-
    Función: obtenerDistanciaTotalBicicleta
    Descripción: obtiene la distancia total recorrida por una bicicleta.
    Objetivo: permitir calcular la distancia total recorrida por una bicicleta.
    Parámetros: 
        - codigoBici: código de la bicicleta.
    Retorno: la distancia total recorrida por la bicicleta.
    Restricciones: --
-}
obtenerDistanciaTotalBicicleta :: String -> IO Double
obtenerDistanciaTotalBicicleta codigoB = do
    facturas <- obtenerFacturasDeBicicleta codigoB
    (obtenerDistanciaTotalBicicleta' facturas)

{-
    Función: obtenerDistanciaTotalBicicleta'
    Descripción: función auxiliar de obtenerDistanciaTotalBicicleta.
    Objetivo: va recorriendo la lista de facturas y va sumando las distancias de las facturas.
    Parámetros:
        - facturas: lista de facturas de la bicicleta
    Retorno:
        - la distancia total recorrida por la bicicleta.
    Restricciones: --
-}
obtenerDistanciaTotalBicicleta' :: [Factura] -> IO Double
obtenerDistanciaTotalBicicleta' [] = return 0
obtenerDistanciaTotalBicicleta' (x:xs) = do
    distancia <- obtenerDistanciaEntreParqueos (parqueoSalida x) (parqueoLlegada x)
    distancia' <- obtenerDistanciaTotalBicicleta' xs
    return (distancia + distancia')

{-
    Función: generarTuplaBiciDistancia
    Descripción: genera una tupla con el código de la bicicleta y la distancia recorrida por la bicicleta.
    Parámetros:
        - una lista de biciDistancia.
    Retorno:
        - una lista de tuplas con el código de la bicicleta y la distancia recorrida por la bicicleta.
    Restricciones: --
-}
generarTuplaBiciDistancia :: [Factura] -> IO [(String, Double)]
generarTuplaBiciDistancia [] = return []
generarTuplaBiciDistancia (x:xs) = do
    distancia <- obtenerDistanciaTotalBicicleta (codigoBici x)
    tupla <- generarTuplaBiciDistancia xs
    return ((codigoBici x, distancia) : tupla)

{-
    Función: generarTuplaUsuarioCantViajes
    Descripción: genera una tupla con el código del usuario y la cantidad de viajes que ha realizado.
    Objetivo: permitir generar una lista de tuplas con el código del usuario y la cantidad de viajes que ha realizado.
    Parámetros:
        - una lista de facturas.
    Retorno:
        - una lista de tuplas con el código del usuario y la cantidad de viajes que ha realizado.
    Restricciones: --
-}
generarTuplaUsuarioCantViajes :: [Factura] -> IO [(String, Int)]
generarTuplaUsuarioCantViajes [] = return []
generarTuplaUsuarioCantViajes (x:xs) = do
    cantViajes <- obtenerCantidadViajesUsuario (cedulaCliente x)
    tupla <- generarTuplaUsuarioCantViajes xs
    return ((cedulaCliente x, cantViajes) : tupla)

{-
    Función: obtenerCantidadViajesUsuario
    Descripción: obtiene la cantidad de viajes que ha realizado un usuario.
    Objetivo: dado un usuario, obtener la cantidad de viajes que ha realizado.
    Parámetros:
        - cedula: cédula del usuario.
    Retorno: 
        - la cantidad de viajes que ha realizado el usuario.
    Restricciones: --
-}
obtenerCantidadViajesUsuario :: String -> IO Int
obtenerCantidadViajesUsuario cedula = do
    facturas <- obtenerFacturas
    return (obtenerCantidadViajesUsuario' cedula facturas)

{-
    Función: obtenerCantidadViajesUsuario'
    Descripción: función auxiliar de obtenerCantidadViajesUsuario.
    Objetivo: va recorriendo la lista de facturas y va sumando 1 por cada factura que tenga el usuario.
    Parámetros:
        - cedula: cédula del usuario.
        - facturas: lista de facturas.
    Retorno:
        - la cantidad de viajes que ha realizado el usuario.
    Restricciones: --
-}
obtenerCantidadViajesUsuario' :: String -> [Factura] -> Int
obtenerCantidadViajesUsuario' cedula [] = 0
obtenerCantidadViajesUsuario' cedula (x:xs)
    | cedula == cedulaCliente x = 1 + obtenerCantidadViajesUsuario' cedula xs
    | otherwise = obtenerCantidadViajesUsuario' cedula xs

{-
    Función: generarTuplaParqueoCantViajes
    Descripción: genera una tupla con el código del parqueo y la cantidad de viajes que ha realizado.
    Parámetros:
        - una lista de facturas.
    Retorno:
        - una lista de tuplas con el código del parqueo y la cantidad de viajes que ha realizado.
    Restricciones: --
-}
generarTuplaParqueoCantViajes :: [Factura] -> IO [(String, Int)]
generarTuplaParqueoCantViajes [] = return []
generarTuplaParqueoCantViajes (x:xs) = do
    cantViajesS <- obtenerCantidadViajesParqueo (parqueoSalida x)
    cantViajesL <- obtenerCantidadViajesParqueo (parqueoLlegada x)
    tupla <- generarTuplaParqueoCantViajes xs
    return ((parqueoSalida x, cantViajesS) : (parqueoLlegada x, cantViajesL) : tupla)

{-
    Función: obtenerCantidadViajesParqueo
    Descripción: obtiene la cantidad de viajes que ha realizado un parqueo.
    Objetivo: dado un parqueo, obtener la cantidad de viajes que ha realizado.
    Parámetros:
        - parqueo: código del parqueo.
    Retorno: 
        - la cantidad de viajes que ha realizado el parqueo.
    Restricciones: --
-}
obtenerCantidadViajesParqueo :: String -> IO Int
obtenerCantidadViajesParqueo parqueo = do
    facturas <- obtenerFacturas
    return (obtenerCantidadViajesParqueo' parqueo facturas)

{-
    Función: obtenerCantidadViajesParqueo'
    Descripción: función auxiliar de obtenerCantidadViajesParqueo.
    Objetivo: va recorriendo la lista de facturas y va sumando 1 por cada factura que tenga el parqueo.
    Parámetros:
        - parqueo: código del parqueo.
        - facturas: lista de facturas.
    Retorno:
        - la cantidad de viajes que ha realizado el parqueo.
    Restricciones: --
-}
obtenerCantidadViajesParqueo' :: String -> [Factura] -> Int
obtenerCantidadViajesParqueo' parqueo [] = 0
obtenerCantidadViajesParqueo' parqueo (x:xs)
    | parqueo == parqueoSalida x || parqueo == parqueoLlegada x = 1 + obtenerCantidadViajesParqueo' parqueo xs
    | otherwise = obtenerCantidadViajesParqueo' parqueo xs

{-
    Función obtenerDistanciaTotalDeCadaBicicleta'
    Descripción: obtiene la distancia total recorrida por cada bicicleta.
    Objetivo: obtener la distancia total recorrida por cada bicicleta.
    Parámetros:
        - facturas: Lista de tuplas con el código de la bicicleta y la distancia recorrida.
    Retorno:
        - Lista de tuplas con el código de la bicicleta y la distancia recorrida.
    Restricciones: --
-}
obtenerDistanciaTotalDeCadaBicicleta' :: [(String, Double)] -> IO [(String, Double)]
obtenerDistanciaTotalDeCadaBicicleta' [] = return []
obtenerDistanciaTotalDeCadaBicicleta' (x:xs) = do
    if (existeTuplaEnLista x xs) then do
        obtenerDistanciaTotalDeCadaBicicleta' xs
    else do
        tupla <- obtenerDistanciaTotalDeCadaBicicleta' xs
        return (x : tupla)

{-
    Función: obtenerTotalViajesDeCadaUsuario
    Descripción: obtiene el total de viajes que ha realizado cada usuario.
    Objetivo: limpiar la lista de facturas y obtener el total de viajes que ha realizado cada usuario,
              quitando repetidos
    Parámetros:
        - facturas: Lista de tuplas con el código de la bicicleta y la distancia recorrida.
    Retorno:
        - Lista de tuplas con el código de la bicicleta y la distancia recorrida.
    Restricciones: --
-}
obtenerTotalViajesDeCadaUsuario' :: [(String, Int)] -> IO [(String, Int)]
obtenerTotalViajesDeCadaUsuario' [] = return []
obtenerTotalViajesDeCadaUsuario' (x:xs) = do
    if (existeTuplaEnListaInt x xs) then do
        obtenerTotalViajesDeCadaUsuario' xs
    else do
        tupla <- obtenerTotalViajesDeCadaUsuario' xs
        return (x : tupla)

{-
    Función: obtenerTotalViajesDeCadaParqueo
    Descripción: obtiene el total de viajes que ha realizado cada parqueo.
    Objetivo: limpiar la lista de facturas y obtener el total de viajes que ha realizado cada parqueo,
              quitando repetidos
    Parámetros:
        - facturas: Lista de tuplas con el código del parqueo y la cantidad de viajes.
    Retorno:
        - Lista de tuplas con el código del parqueo y la cantidad de viajes.
    Restricciones: --
-}
obtenerTotalViajesDeCadaParqueo' :: [(String, Int)] -> IO [(String, Int)]
obtenerTotalViajesDeCadaParqueo' [] = return []
obtenerTotalViajesDeCadaParqueo' (x:xs) = do
    if (existeTuplaEnListaInt x xs) then do
        obtenerTotalViajesDeCadaParqueo' xs
    else do
        tupla <- obtenerTotalViajesDeCadaParqueo' xs
        return (x : tupla)

{-
    Función: obtenerMayorListaTuplas
    Descripción: obtiene el mayor valor de una lista de tuplas.
    Objetivo: obtener el mayor valor de una lista de tuplas. (con su valor numérico)
    Parámetros:
        - lista: lista de tuplas.
    Retorno:
        - la tupla con el mayor valor.
    Restricciones: --
-}
obtenerMayorListaTuplas :: [(String, Double)] -> (String, Double)
obtenerMayorListaTuplas [] = ("", 0)
obtenerMayorListaTuplas (x:xs) = obtenerMayorListaTuplas' x xs

{-
    Función: obtenerMayorListaTuplas'
    Descripción: función auxiliar de obtenerMayorListaTuplas.
    Objetivo: va recorriendo la lista de tuplas y va comparando el valor numérico de cada tupla.
    Parámetros:
        - tupla: tupla con el mayor valor.
        - lista: lista de tuplas.
    Retorno:
        - la tupla con el mayor valor.
    Restricciones: --
-}
obtenerMayorListaTuplas' :: (String, Double) -> [(String, Double)] -> (String, Double)
obtenerMayorListaTuplas' mayor [] = mayor
obtenerMayorListaTuplas' mayor (x:xs)
    | snd mayor < snd x = obtenerMayorListaTuplas' x xs
    | otherwise = obtenerMayorListaTuplas' mayor xs

{-
    Función: obtenerMayorListaTuplasInt
    Descripción: obtiene el mayor valor de una lista de tuplas con un entero.
    Objetivo: obtener el mayor valor de una lista de tuplas. (con su valor numérico entero)
    Parámetros:
        - lista: lista de tuplas.
    Retorno:
        - la tupla con el mayor valor.
    Restricciones: --
-}
obtenerMayorListaTuplasInt :: [(String, Int)] -> (String, Int)
obtenerMayorListaTuplasInt [] = ("", 0)
obtenerMayorListaTuplasInt (x:xs) = obtenerMayorListaTuplasInt' x xs

{-
    Función: obtenerMayorListaTuplasInt'
    Descripción: función auxiliar de obtenerMayorListaTuplasInt.
    Objetivo: va recorriendo la lista de tuplas y va comparando el valor numérico entero de cada tupla.
    Parámetros:
        - tupla: tupla con el mayor valor entero.
        - lista: lista de tuplas.
    Retorno:
        - la tupla con el mayor valor.
    Restricciones: --
-}
obtenerMayorListaTuplasInt' :: (String, Int) -> [(String, Int)] -> (String, Int)
obtenerMayorListaTuplasInt' mayor [] = mayor
obtenerMayorListaTuplasInt' mayor (x:xs)
    | snd mayor < snd x = obtenerMayorListaTuplasInt' x xs
    | otherwise = obtenerMayorListaTuplasInt' mayor xs

{-
    Función: eliminarTuplaDeLista
    Descripción: elimina una tupla de una lista de tuplas.
    Objetivo: eliminar una tupla de una lista de tuplas.
    Parámetros:
        - tupla: tupla a eliminar.
        - lista: lista de tuplas.
    Retorno:
        - lista de tuplas sin la tupla eliminada.
    Restricciones: --
-}
eliminarTuplaDeLista :: (String, Double) -> [(String, Double)] -> [(String, Double)]
eliminarTuplaDeLista tupla [] = []
eliminarTuplaDeLista tupla (x:xs)
    | x == tupla = xs
    | otherwise = x : eliminarTuplaDeLista tupla xs

{-
    Función: eliminarTuplaDeListaInt
    Descripción: elimina una tupla de una lista de tuplas con un entero.
    Objetivo: eliminar una tupla de una lista de tuplas.
    Parámetros:
        - tupla: tupla a eliminar.
        - lista: lista de tuplas.
    Retorno:
        - lista de tuplas sin la tupla eliminada.
    Restricciones: --
-}
eliminarTuplaDeListaInt :: (String, Int) -> [(String, Int)] -> [(String, Int)]
eliminarTuplaDeListaInt tupla [] = []
eliminarTuplaDeListaInt tupla (x:xs)
    | x == tupla = xs
    | otherwise = x : eliminarTuplaDeListaInt tupla xs

{-
    Función mostrarTopTresBicisConMasDistancia
    Descripción: muestra el top 3 de bicicletas con más distancia recorrida.
    Objetivo: mostrar el top 3 de bicicletas con más distancia recorrida.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
mostrarTopTresBicisConMasDistancia :: IO ()
mostrarTopTresBicisConMasDistancia = do
    facturas <- obtenerFacturas
    tuplas <- generarTuplaBiciDistancia facturas
    tuplas' <- obtenerDistanciaTotalDeCadaBicicleta' tuplas
    let mayor1 = obtenerMayorListaTuplas tuplas'
    let tuplas'' = eliminarTuplaDeLista mayor1 tuplas'
    let mayor2 = obtenerMayorListaTuplas tuplas''
    let tuplas''' = eliminarTuplaDeLista mayor2 tuplas''
    let mayor3 = obtenerMayorListaTuplas tuplas'''
    putStrLn ("Top 3 de bicicletas con más distancia recorrida:\n" ++
              "1. " ++ fst mayor1 ++ " - " ++ show (snd mayor1) ++ " km\n" ++
              "2. " ++ fst mayor2 ++ " - " ++ show (snd mayor2) ++ " km\n" ++
              "3. " ++ fst mayor3 ++ " - " ++ show (snd mayor3) ++ " km\n")

{-
    Función mostrarTopCincoUsuariosConMasViajes
    Descripción: muestra el top 5 de usuarios con más viajes.
    Objetivo: mostrar el top 5 de usuarios con más viajes.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
mostrarTopCincoUsuariosConMasViajes :: IO ()
mostrarTopCincoUsuariosConMasViajes = do
    facturas <- obtenerFacturas
    tuplas <- generarTuplaUsuarioCantViajes facturas
    tuplas' <- obtenerTotalViajesDeCadaUsuario' tuplas
    let mayor1 = obtenerMayorListaTuplasInt tuplas'
    let tuplas'' = eliminarTuplaDeListaInt mayor1 tuplas'
    let mayor2 = obtenerMayorListaTuplasInt tuplas''
    let tuplas''' = eliminarTuplaDeListaInt mayor2 tuplas''
    let mayor3 = obtenerMayorListaTuplasInt tuplas'''
    let tuplas'''' = eliminarTuplaDeListaInt mayor3 tuplas'''
    let mayor4 = obtenerMayorListaTuplasInt tuplas''''
    let tuplas''''' = eliminarTuplaDeListaInt mayor4 tuplas'''' 
    let mayor5 = obtenerMayorListaTuplasInt tuplas'''''
    putStrLn ("Top 5 de usuarios con más viajes:\n" ++
              "1. " ++ fst mayor1 ++ " - " ++ show (snd mayor1) ++ " viajes\n" ++
              "2. " ++ fst mayor2 ++ " - " ++ show (snd mayor2) ++ " viajes\n" ++
              "3. " ++ fst mayor3 ++ " - " ++ show (snd mayor3) ++ " viajes\n" ++
              "4. " ++ fst mayor4 ++ " - " ++ show (snd mayor4) ++ " viajes\n" ++
              "5. " ++ fst mayor5 ++ " - " ++ show (snd mayor5) ++ " viajes\n")

{-
    Función: mostrarTopCincoParqueosConMasViajes
    Descripción: muestra el top 5 de parqueos con más viajes.
    Objetivo: mostrar el top 5 de parqueos con más viajes.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
mostrarTopCincoParqueosConMasViajes :: IO ()
mostrarTopCincoParqueosConMasViajes = do
    facturas <- obtenerFacturas
    tuplas <- generarTuplaParqueoCantViajes facturas
    tuplas' <- obtenerTotalViajesDeCadaParqueo' tuplas
    let mayor1 = obtenerMayorListaTuplasInt tuplas'
    let tuplas'' = eliminarTuplaDeListaInt mayor1 tuplas'
    let mayor2 = obtenerMayorListaTuplasInt tuplas''
    let tuplas''' = eliminarTuplaDeListaInt mayor2 tuplas''
    let mayor3 = obtenerMayorListaTuplasInt tuplas'''
    let tuplas'''' = eliminarTuplaDeListaInt mayor3 tuplas'''
    let mayor4 = obtenerMayorListaTuplasInt tuplas''''
    let tuplas''''' = eliminarTuplaDeListaInt mayor4 tuplas'''' 
    let mayor5 = obtenerMayorListaTuplasInt tuplas'''''
    putStrLn ("Top 5 de parqueos con más viajes:\n" ++
              "1. " ++ fst mayor1 ++ " - " ++ show (snd mayor1) ++ " viajes\n" ++
              "2. " ++ fst mayor2 ++ " - " ++ show (snd mayor2) ++ " viajes\n" ++
              "3. " ++ fst mayor3 ++ " - " ++ show (snd mayor3) ++ " viajes\n" ++
              "4. " ++ fst mayor4 ++ " - " ++ show (snd mayor4) ++ " viajes\n" ++
              "5. " ++ fst mayor5 ++ " - " ++ show (snd mayor5) ++ " viajes\n")

{-
    Función: mostrarTotalRecorridoPorTodasLasBicicletas
    Descripción: muestra el total de recorrido de todas las bicicletas.
    Objetivo: mostrar el total de recorrido de todas las bicicletas.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
mostrarTotalRecorridoPorTodasLasBicicletas :: IO ()
mostrarTotalRecorridoPorTodasLasBicicletas = do
    facturas <- obtenerFacturas
    tuplas <- generarTuplaBiciDistancia facturas
    tuplas' <- obtenerDistanciaTotalDeCadaBicicleta' tuplas
    let total = sumarDistanciasTuplas tuplas'
    putStrLn ("Total de distancia recorrida por todas las bicicletas: " ++ show total ++ " km")

{-
    Función: sumarPreciosFacturas
    Descripción: suma lo pagado de todas las facturas.
    Objetivo: sumar lo pagado de todas las facturas.
    Parámetros:
        facturas: lista de facturas.
    Retorno: total de lo pagado de todas las facturas.
    Restricciones: --
-}
sumarPreciosFacturas :: [Factura] -> IO Double
sumarPreciosFacturas [] = return 0
sumarPreciosFacturas (x:xs) = do
    parqueoSal <- Parqueo.obtenerParqueo "./src/data/parqueo.json" (parqueoSalida x)
    parqueoLleg <- Parqueo.obtenerParqueo "./src/data/parqueo.json" (parqueoLlegada x)
    let distancia = Parqueo.distanciaEntreDosParqueos parqueoSal parqueoLleg
    bicFactura <- Bicicleta.obtenerBicicleta (codigoBici x) "./src/data/bicicletas.json"
    tarifa <- Comercio.obtenerTarifaSegunTipo (Bicicleta.tipo bicFactura)
    let costoTotal = (distancia * tarifa)
    costoTotal' <- sumarPreciosFacturas xs
    return (costoTotal + costoTotal')

{-
    Función: mostrarTotalFacturadoPorTodasLasBicicletas
    Descripción: muestra el total facturado por todas las bicicletas.
    Objetivo: mostrar el total facturado por todas las bicicletas.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
mostrarTotalFacturadoPorTodasLasBicicletas :: IO ()
mostrarTotalFacturadoPorTodasLasBicicletas = do
    facturas <- obtenerFacturas
    totalFacturado <- sumarPreciosFacturas facturas
    putStrLn ("Total facturado por todas las bicicletas: " ++ show totalFacturado ++ " $.")

{-
    Función: mostrarResumen
    Descripción: muestra el resumen del negocio.
    Objetivo: Muestra el total de viajes, total recorrido por bicis y el totcal facturado.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
mostrarResumen :: IO ()
mostrarResumen = do
    mostrarTotalViajes
    mostrarTotalRecorridoPorTodasLasBicicletas
    mostrarTotalFacturadoPorTodasLasBicicletas

{-
    Función: sumarDistanciasTuplas
    Descripción: suma las distancias de una lista de tuplas.
    Objetivo: sumar las distancias de una lista de tuplas. Para luego mostrar el total de recorrido de todas las bicicletas.
    Parámetros:
        tuplas: lista de tuplas. (Codigo, Distancia)
    Retorno:
        total: total de la suma de las distancias de las tuplas.
    Restricciones: --
-}
sumarDistanciasTuplas :: [(String, Double)] -> Double
sumarDistanciasTuplas [] = 0
sumarDistanciasTuplas (x:xs) = snd x + sumarDistanciasTuplas xs

{-
    Función: existeTuplaEnLista
    Descripción: verifica si existe una tupla en una lista de tuplas con double.
    Objetivo: verificar si existe una tupla en una lista de tuplas con double. Para luego mostrar el total de recorrido de todas las bicicletas.
    Parámetros:
        tupla: tupla a verificar. (Codigo, Distancia)
        tuplas: lista de tuplas. (Codigo, Distancia)
    Retorno:
        True: si existe la tupla en la lista.
        False: si no existe la tupla en la lista.
    Restricciones: --
-}
existeTuplaEnLista :: (String, Double) -> [(String, Double)] -> Bool
existeTuplaEnLista tupla [] = False
existeTuplaEnLista tupla (x:xs)
    | (fst tupla) == (fst x) = True
    | otherwise = existeTuplaEnLista tupla xs

{-
    Función: existeTuplaEnLista
    Descripción: verifica si existe una tupla en una lista de tuplas con enteros.
    Objetivo: verificar si existe una tupla en una lista de tuplas con enteros. Para luego mostrar el total de viajes de cada parqueo.
    Parámetros:
        tupla: tupla a verificar. (Codigo, CantidadViajes)
        tuplas: lista de tuplas. (Codigo, CantidadViajes)
    Retorno:
        True: si existe la tupla en la lista.
        False: si no existe la tupla en la lista.
    Restricciones: --
-}
existeTuplaEnListaInt :: (String, Int) -> [(String, Int)] -> Bool
existeTuplaEnListaInt tupla [] = False
existeTuplaEnListaInt tupla (x:xs)
    | (fst tupla) == (fst x) = True
    | otherwise = existeTuplaEnListaInt tupla xs

{-
    Función: mostrarTotalViajes
    Descripción: muestra el total de viajes realizados
    Objetivo: mostrar el total de viajes realizados.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
mostrarTotalViajes :: IO ()
mostrarTotalViajes = do
    facturas <- obtenerFacturas
    putStrLn ("Total de viajes: " ++ show (length facturas))

{-
    Función: existeFactura
    Descripción: verifica si existe una factura en una lista de facturas.
    Objetivo: verificar si existe una factura en una lista de facturas.
    Parámetros:
        idFactura : id de la factura a verificar.
    Retorno:
        True: si existe la factura en la lista.
        False: si no existe la factura en la lista.
    Restricciones: --
-}
existeFactura :: Int -> IO Bool
existeFactura idFactura = do
    facturas <- obtenerFacturas
    return (existeFactura' idFactura facturas)

{-
    Función: existeFactura'
    Descripción: verifica si existe una factura en una lista de facturas.
    Objetivo: verificar si existe una factura en una lista de facturas.
    Parámetros:
        idFactura : id de la factura a verificar.
        facturas: lista de facturas.
    Retorno:
        True: si existe la factura en la lista.
        False: si no existe la factura en la lista.
    Restricciones: --
-}
existeFactura' :: Int -> [Factura] -> Bool
existeFactura' idF [] = False
existeFactura' idF (x:xs)
    | idF == (idFactura x) = True
    | otherwise = existeFactura' idF xs

{-
    Función: obtenerFactura
    Descripción: dada un id de factura, obtiene la factura.
    Objetivo: obtener la factura de un id de factura.
    Parámetros:
        idFactura : id de la factura a obtener.
    Retorno:
        factura: factura con el id especificado.
    Restricciones: --
-}
obtenerFactura :: Int -> IO Factura
obtenerFactura idFactura = do
    facturas <- obtenerFacturas
    return (obtenerFactura' idFactura facturas)

{-
    Función: obtenerFactura'
    Descripción: dada un id de factura, obtiene la factura.
    Objetivo: obtener la factura de un id de factura.
    Parámetros:
        idFactura : id de la factura a obtener.
        facturas: lista de facturas.
    Retorno:
        factura: factura con el id especificado.
    Restricciones: --
-}
obtenerFactura' :: Int -> [Factura] -> Factura
obtenerFactura' idF [] = Factura 0 "" "" "" "" ""
obtenerFactura' idF (x:xs)
    | idF == (idFactura x) = x
    | otherwise = obtenerFactura' idF xs
