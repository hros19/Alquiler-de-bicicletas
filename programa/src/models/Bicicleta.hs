{-# LANGUAGE DeriveGeneric #-}

module Bicicleta where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import GHC.Generics
import qualified Data.ByteString.Lazy as BS

-- Variables globales

path_bicicletas :: FilePath
path_bicicletas = "./src/data/bicicletas.json"

{--------------------------------------------------------------------------------------------------
Modelo: Bicicleta
Versión: 1.0 (24 de setiembre del 2022)
Autor: Hansol Antay Rostrán (https://github.com/hros19)
---------------------------------------------------------------------------------------------------
Descripción y propósito: 
    Representará las bicicletas que se encuentran en el sistema. Su propósito es poder
    almacenar y manipular la información de las bicicletas.
Atributos:
    codigo: Código de la bicicleta.
    tipo: Tipo de bicicleta. (TR, AE)
    parqueo: Parqueo en el que se encuentra la bicicleta.
--------------------------------------------------------------------------------------------------}

data Bicicleta = Bicicleta {
    codigo :: String,
    tipo :: String,
    parqueo :: String
} deriving (Show, Generic)

instance FromJSON Bicicleta
instance ToJSON Bicicleta

{-
    Función: obtenerBicicletas
    Descripción: obtiene todas las bicicletas dato un path de archivo.
    Objetivo: obtener en forma de lista las bicicletas.
    Parámetros: path del archivo.
    Retorno: lista de bicicletas.
    Restricciones: --
-}
obtenerBicicletas :: String -> IO [Bicicleta]
obtenerBicicletas path = do
    contenido <- BS.readFile path
    let bicicletas = decode contenido :: Maybe [Bicicleta]
    case bicicletas of
        Nothing -> return []
        Just bicicletas -> return bicicletas

{-
    Función: obtenerBicicleta
    Descripción: obtiene una bicicleta dado un código y un path de archivo.
    Objetivo: obtiene un objeto Bicicleta dado un código y un path de archivo.
    Parámetros:
        - path: path del archivo.
        - codigo: código de la bicicleta.
    Retorno: objeto Bicicleta en caso de que exista, bicicleta "null" en caso contrario.
    Restricciones: --
-}
obtenerBicicleta :: String -> String -> IO Bicicleta
obtenerBicicleta codigo path = do
    bicicletas <- obtenerBicicletas path
    case bicicletas of
        [] -> return (Bicicleta "" "" "")
        _ -> return (obtenerBicicleta' codigo bicicletas)

{-
    Función: obtenerBicicleta' (Auxiliar)
    Descripción: obtiene un objeto bicicleta buscando en una lista de bicicletas.
    Objetivo: iterar la lista de bicicletas y obtener la bicicleta que se busca.
    Parámetros:
        - codigo: código de la bicicleta.
        - bicicletas: lista de bicicletas.
    Retorno: objeto Bicicleta en caso de que exista, bicicleta "null" en caso contrario.
    Restricciones: --
-}
obtenerBicicleta' :: String -> [Bicicleta] -> Bicicleta
obtenerBicicleta' codigo [] = Bicicleta "" "" ""
obtenerBicicleta' codigo (x:xs) 
    | (codigo == (Bicicleta.codigo x)) = x
    | otherwise = obtenerBicicleta' codigo xs

{-
    Función: eliminarBicicleta
    Descripción: elimina una bicicleta dado su código y un path de archivo.
    Objetivo: eliminar una bicicleta en específico según su código y un path de archivo.
    Parámetros:
        - path: path del archivo.
        - codigo: código de la bicicleta.
    Retorno: --
    Restricciones: --
-}
eliminarBicicleta :: String -> String -> IO ()
eliminarBicicleta codigo path = do
    bicicletas <- obtenerBicicletas path
    let bicicletas' = eliminarBicicleta' codigo bicicletas
    BS.writeFile path_bicicletas (encodePretty bicicletas')

{-
    Función: eliminarBicicleta' (Auxiliar)
    Descripción: función auxiliar que permite eliminar una bicicleta de una lista de bicicletas
    Objetivo: eliminar una bicicleta de una lista de bicicletas.
    Parámetros:
        - codigo: código de la bicicleta.
        - bicicletas: lista de bicicletas.
    Retorno: lista de bicicletas sin la bicicleta eliminada.
    Restricciones: --
-}
eliminarBicicleta' :: String -> [Bicicleta] -> [Bicicleta]
eliminarBicicleta' codigo [] = []
eliminarBicicleta' codigo (x:xs)
    | codigo == (Bicicleta.codigo x) = xs
    | otherwise = x : eliminarBicicleta' codigo xs

{-
    Función: actualizarBicicleta
    Descripción: actualiza una bicicleta que se encuentre registrada en un archivo.
    Objetivo: poder modificar los datos de una bicicleta
    Parámetros:
        - path: path del archivo.
        - bicicleta: objeto bicicleta con los datos actualizados.
    Retorno: --
    Restricciones: --
-}
actualizarBicicleta :: Bicicleta -> String -> IO ()
actualizarBicicleta bicicleta path = do
    bicicletas <- obtenerBicicletas path
    let bicicletas' = actualizarBicicleta' bicicleta bicicletas
    BS.writeFile path_bicicletas (encodePretty bicicletas')

{-
    Función: actualizarBicicleta' (Auxiliar)
    Descripción: función auxiliar que permite actualizar una bicicleta de una lista de bicicletas
    Objetivo: actualizar una bicicleta de una lista de bicicletas.
    Parámetros:
        - bicicleta: objeto bicicleta con los datos actualizados.
        - bicicletas: lista de bicicletas.
    Retorno: lista de bicicletas con la bicicleta actualizada.
    Restricciones: --
-}
actualizarBicicleta' :: Bicicleta -> [Bicicleta] -> [Bicicleta]
actualizarBicicleta' bicicleta [] = []
actualizarBicicleta' bicicleta (x:xs)
    | (Bicicleta.codigo bicicleta) == (Bicicleta.codigo x) = bicicleta : xs
    | otherwise = x : actualizarBicicleta' bicicleta xs

{-
    Función: mostrarBicicleta
    Descripción: retorna una cadena de caracteres con los datos de una bicicleta.
    Objetivo: mostrar los datos de una bicicleta.
    Parámetros:
        - bicicleta: objeto bicicleta.
    Retorno: cadena de caracteres con los datos de la bicicleta.
    Restricciones: --
-}
mostrarBicicleta :: Bicicleta -> String
mostrarBicicleta bicicleta 
    | (Bicicleta.parqueo bicicleta) == "En tránsito" = ""
    | otherwise = "Código: " ++ (Bicicleta.codigo bicicleta) ++ "\n" ++
                  "Tipo: " ++ (Bicicleta.tipo bicicleta) ++ "\n" ++
                  "Parqueo: " ++ (Bicicleta.parqueo bicicleta) ++ "\n" ++
                  "--------------------------------------------\n"

{-
    Función: mostrarBicicletaSinParqueo
    Descripción: retorna una cadena de caracteres con los datos de una bicicleta sin el parqueo.
    Objetivo: mostrar los datos de una bicicleta sin el parqueo.
    Parámetros:
        - bicicleta: objeto bicicleta.
    Retorno: cadena de caracteres con los datos de la bicicleta sin el parqueo.
    Restricciones: --
-}
mostrarBicicletaSinParqueo :: Bicicleta -> String
mostrarBicicletaSinParqueo bicicleta 
    | (Bicicleta.parqueo bicicleta) == "En tránsito" = ""
    | otherwise = "Código: " ++ (Bicicleta.codigo bicicleta) ++ "\n" ++
                  "Tipo: " ++ (Bicicleta.tipo bicicleta) ++ "\n" ++
                  "--------------------------------------------\n"

{-
    Función: mostrarBicicletaEnTransito
    Descripción: retorna una cadena de caracteres con los datos de una bicicleta incluyendo el parqueo.
    Parámetros:
        - bicicleta: objeto bicicleta.
    Retorno: cadena de caracteres con los datos de la bicicleta incluyendo el parqueo.
    Restricciones: --
-}
mostrarBicicletaEnTransito :: Bicicleta -> String
mostrarBicicletaEnTransito bicicleta = "Código: " ++ (Bicicleta.codigo bicicleta) ++ "\n" ++
                                       "Tipo: " ++ (Bicicleta.tipo bicicleta) ++ "\n" ++
                                       "Parqueo: " ++ (Bicicleta.parqueo bicicleta) ++ "\n" ++
                                       "--------------------------------------------\n"

{-
    Función: mostrarBicicletas
    Descripción: muestra las bicicletas registradas en un archivo.
    Objetivo: mostrar las bicicletas registradas en un archivo.
    Parámetros:
        -  path: path del archivo.
    Retorno: --
    Restricciones: --
-}
mostrarBicicletas :: String -> IO ()
mostrarBicicletas path = do
    bicicletas <- obtenerBicicletas path
    putStrLn (mostrarBicicletas' bicicletas)

{-
    Función: mostrarBicicletas'
    Descripción: función auxiliar que permite mostrar las bicicletas de una lista de bicicletas.
    Objetivo: una vez obtenidas las bicicletas de un archivo, mostrarlas.
    Parámetros:
        - bicicletas: lista de bicicletas.
    Retorno: cadena de caracteres con los datos de las bicicletas.
    Restricciones: --
-}
mostrarBicicletas' :: [Bicicleta] -> String
mostrarBicicletas' [] = ""
mostrarBicicletas' (x:xs) = (mostrarBicicleta x) ++ (mostrarBicicletas' xs)

{-
    Función: mostrarBicicletasEnTransito
    Descripción: muestra todas las bicicletas que encuentran en tránsito en un archivo.
    Objetivo: mostrar las bicicletas que encuentran en tránsito de una ruta de archivo.
    Parámetros: 
        - path: ruta del archivo.
    Retorno: --
    Restricciones: --
-}
mostrarBicicletasEnTransito :: String -> IO ()
mostrarBicicletasEnTransito path = do
    bicicletas <- obtenerBicicletas path
    putStrLn (mostrarBicicletasEnTransito' bicicletas)

{-
    Función: mostrarBicicletasEnTransito'
    Descripción: función auxiliar que permite mostrar las bicicletas que encuentran en tránsito de una lista de bicicletas.
    Objetivo: una vez obtenidas las bicicletas de un archivo, mostrarlas.
    Parámetros:
        - bicicletas: lista de bicicletas.
    Retorno: cadena de caracteres con los datos de las bicicletas.
    Restricciones: --
-}
mostrarBicicletasEnTransito' :: [Bicicleta] -> String
mostrarBicicletasEnTransito' [] = ""
mostrarBicicletasEnTransito' (x:xs)
    | (Bicicleta.parqueo x) == "En tránsito" = (mostrarBicicletaEnTransito x) ++
                                               (mostrarBicicletasEnTransito' xs)
    | otherwise = mostrarBicicletasEnTransito' xs

{-
    Función: mostrarBicicletasDeParqueo
    Descripción: muestra todas las bicicletas que se encuentran en un parqueo en un archivo.
    Objetivo: mostrar las bicicletas que se encuentran en un parqueo de una ruta de archivo.
    Parámetros:
        -   path: ruta del archivo.
        -   parqueo: nombre del parqueo.
    Retorno: --
    Restricciones: --
-}
mostrarBicicletasDeParqueo :: String -> String -> IO ()
mostrarBicicletasDeParqueo parqueo path = do
    bicicletas <- obtenerBicicletas path
    putStrLn (mostrarBicicletasDeParqueo' parqueo bicicletas)

{-
    Función: mostrarBicicletasDeParqueo'
    Descripción: función auxiliar que permite mostrar las bicicletas que se encuentran en un parqueo de una lista de bicicletas.
    Objetivo: una vez obtenidas las bicicletas de un archivo, mostrarlas.
    Parámetros:
        - parqueo: nombre del parqueo.
        - bicicletas: lista de bicicletas.
    Retorno: cadena de caracteres con los datos de las bicicletas.
    Restricciones: --
-}
mostrarBicicletasDeParqueo' :: String -> [Bicicleta] -> String
mostrarBicicletasDeParqueo' parqueo [] = ""
mostrarBicicletasDeParqueo' parqueo (x:xs)
    | parqueo == (Bicicleta.parqueo x) = (mostrarBicicletaSinParqueo x) ++
                                         (mostrarBicicletasDeParqueo' parqueo xs)
    | otherwise = mostrarBicicletasDeParqueo' parqueo xs

{-
    Función: mostrarBicicletasPorTipo
    Descripción: muestra todas las bicicletas de un tipo en un archivo.
    Objetivo: mostrar las bicicletas de un tipo de una ruta de archivo.
    Parámetros:
        -   path: ruta del archivo.
        -   tipo: tipo de bicicleta.
    Retorno: --
    Restricciones: --
-}
mostrarBicicletasPorTipo :: String -> String -> IO ()
mostrarBicicletasPorTipo tipo path = do
    bicicletas <- obtenerBicicletas path
    putStrLn (mostrarBicicletasPorTipo' tipo bicicletas)

{-
    Función: mostrarBicicletasPorTipo'
    Descripción: función auxiliar que permite mostrar las bicicletas de un tipo de una lista de bicicletas.
    Objetivo: una vez obtenidas las bicicletas de un archivo, mostrarlas.
    Parámetros:
        - tipo: tipo de bicicleta.
        - bicicletas: lista de bicicletas.
    Retorno: cadena de caracteres con los datos de las bicicletas.
    Restricciones: --
-}
mostrarBicicletasPorTipo' :: String -> [Bicicleta] -> String
mostrarBicicletasPorTipo' tipo [] = ""
mostrarBicicletasPorTipo' tipo (x:xs)
    | tipo == (Bicicleta.tipo x) = (mostrarBicicleta x) ++
                                   (mostrarBicicletasPorTipo' tipo xs)
    | otherwise = mostrarBicicletasPorTipo' tipo xs

{-
    Función: bicicletaPerteneceAParqueo
    Descripción: verifica si una bicicleta pertenece a un parqueo.
    Objetivo: verificar si una bicicleta pertenece a un parqueo.
    Parámetros:
        -   codigo: código de la bicicleta.
        -   parqueo: nombre del parqueo.
        -   path: ruta del archivo.
    Retorno: True si la bicicleta pertenece al parqueo, False en caso contrario.
    Restricciones: --
-}
bicicletaPerteneceAParqueo :: String -> String -> String -> IO Bool
bicicletaPerteneceAParqueo codigo parqueo path = do
    bicicletas <- obtenerBicicletas path
    return (bicicletaPerteneceAParqueo' codigo parqueo bicicletas)

{-
    Función: bicicletaPerteneceAParqueo'
    Descripción: solicita todas las bicicletas de un parqueo.
    Objetivo: función auxiliar que permite obtener las bicicletas de un parqueo de una lista de bicicletas.
    Parámetros:
        -   codigo: código de la bicicleta.
        -   parqueo: nombre del parqueo.
        -   bicicletas: lista de bicicletas.
    Retorno: True si la bicicleta pertenece al parqueo, False en caso contrario.
    Restricciones: --
-}
bicicletaPerteneceAParqueo' :: String -> String -> [Bicicleta] -> Bool
bicicletaPerteneceAParqueo' codigo parqueo [] = False
bicicletaPerteneceAParqueo' codigo parqueo (x:xs)
    | (codigo == (Bicicleta.codigo x)) && (parqueo == (Bicicleta.parqueo x)) = True
    | otherwise = bicicletaPerteneceAParqueo' codigo parqueo xs

{-
    Función: cambiarParqueo
    Descripción: función que permite cambiar el parqueo de una bicicleta en un archivo.
    Objetivo: cambiar el parqueo de una bicicleta en un archivo.
    Parámetros:
        -   codigo: código de la bicicleta.
        -   parqueo: nombre del parqueo nuevo.
        -   path: ruta del archivo.
    Retorno: --
    Restricciones: --
-}
cambiarParqueo :: Bicicleta -> String -> String -> IO ()
cambiarParqueo bicicleta parqueo path = do
    let bicicleta' = Bicicleta (Bicicleta.codigo bicicleta) (Bicicleta.tipo bicicleta) parqueo
    actualizarBicicleta bicicleta' path
