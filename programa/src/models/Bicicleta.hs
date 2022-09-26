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

obtenerBicicletas :: String -> IO [Bicicleta]
obtenerBicicletas path = do
    contenido <- BS.readFile path
    let bicicletas = decode contenido :: Maybe [Bicicleta]
    case bicicletas of
        Nothing -> return []
        Just bicicletas -> return bicicletas

obtenerBicicleta :: String -> String -> IO Bicicleta
obtenerBicicleta codigo path = do
    bicicletas <- obtenerBicicletas path
    case bicicletas of
        [] -> return (Bicicleta "" "" "")
        _ -> return (obtenerBicicleta' codigo bicicletas)


obtenerBicicleta' :: String -> [Bicicleta] -> Bicicleta
obtenerBicicleta' codigo [] = Bicicleta "" "" ""
obtenerBicicleta' codigo (x:xs) 
    | (codigo == (Bicicleta.codigo x)) = x
    | otherwise = obtenerBicicleta' codigo xs

eliminarBicicleta :: String -> String -> IO ()
eliminarBicicleta codigo path = do
    bicicletas <- obtenerBicicletas path
    let bicicletas' = eliminarBicicleta' codigo bicicletas
    BS.writeFile path_bicicletas (encodePretty bicicletas')

eliminarBicicleta' :: String -> [Bicicleta] -> [Bicicleta]
eliminarBicicleta' codigo [] = []
eliminarBicicleta' codigo (x:xs)
    | codigo == (Bicicleta.codigo x) = xs
    | otherwise = x : eliminarBicicleta' codigo xs

actualizarBicicleta :: Bicicleta -> String -> IO ()
actualizarBicicleta bicicleta path = do
    bicicletas <- obtenerBicicletas path
    let bicicletas' = actualizarBicicleta' bicicleta bicicletas
    BS.writeFile path_bicicletas (encodePretty bicicletas')

actualizarBicicleta' :: Bicicleta -> [Bicicleta] -> [Bicicleta]
actualizarBicicleta' bicicleta [] = []
actualizarBicicleta' bicicleta (x:xs)
    | (Bicicleta.codigo bicicleta) == (Bicicleta.codigo x) = bicicleta : xs
    | otherwise = x : actualizarBicicleta' bicicleta xs

mostrarBicicleta :: Bicicleta -> String
mostrarBicicleta bicicleta 
    | (Bicicleta.parqueo bicicleta) == "En tránsito" = ""
    | otherwise = "Código: " ++ (Bicicleta.codigo bicicleta) ++ "\n" ++
                  "Tipo: " ++ (Bicicleta.tipo bicicleta) ++ "\n" ++
                  "Parqueo: " ++ (Bicicleta.parqueo bicicleta) ++ "\n" ++
                  "--------------------------------------------\n"

mostrarBicicletaSinParqueo :: Bicicleta -> String
mostrarBicicletaSinParqueo bicicleta 
    | (Bicicleta.parqueo bicicleta) == "En tránsito" = ""
    | otherwise = "Código: " ++ (Bicicleta.codigo bicicleta) ++ "\n" ++
                  "Tipo: " ++ (Bicicleta.tipo bicicleta) ++ "\n" ++
                  "--------------------------------------------\n"


mostrarBicicletaEnTransito :: Bicicleta -> String
mostrarBicicletaEnTransito bicicleta = "Código: " ++ (Bicicleta.codigo bicicleta) ++ "\n" ++
                                       "Tipo: " ++ (Bicicleta.tipo bicicleta) ++ "\n" ++
                                       "Parqueo: " ++ (Bicicleta.parqueo bicicleta) ++ "\n" ++
                                       "--------------------------------------------\n"

mostrarBicicletas :: String -> IO ()
mostrarBicicletas path = do
    bicicletas <- obtenerBicicletas path
    putStrLn (mostrarBicicletas' bicicletas)

mostrarBicicletas' :: [Bicicleta] -> String
mostrarBicicletas' [] = ""
mostrarBicicletas' (x:xs) = (mostrarBicicleta x) ++ (mostrarBicicletas' xs)

mostrarBicicletasEnTransito :: String -> IO ()
mostrarBicicletasEnTransito path = do
    bicicletas <- obtenerBicicletas path
    putStrLn (mostrarBicicletasEnTransito' bicicletas)

mostrarBicicletasEnTransito' :: [Bicicleta] -> String
mostrarBicicletasEnTransito' [] = ""
mostrarBicicletasEnTransito' (x:xs)
    | (Bicicleta.parqueo x) == "En tránsito" = (mostrarBicicletaEnTransito x) ++
                                               (mostrarBicicletasEnTransito' xs)
    | otherwise = mostrarBicicletasEnTransito' xs

mostrarBicicletasDeParqueo :: String -> String -> IO ()
mostrarBicicletasDeParqueo parqueo path = do
    bicicletas <- obtenerBicicletas path
    putStrLn (mostrarBicicletasDeParqueo' parqueo bicicletas)

mostrarBicicletasDeParqueo' :: String -> [Bicicleta] -> String
mostrarBicicletasDeParqueo' parqueo [] = ""
mostrarBicicletasDeParqueo' parqueo (x:xs)
    | parqueo == (Bicicleta.parqueo x) = (mostrarBicicletaSinParqueo x) ++
                                         (mostrarBicicletasDeParqueo' parqueo xs)
    | otherwise = mostrarBicicletasDeParqueo' parqueo xs

mostrarBicicletasPorTipo :: String -> String -> IO ()
mostrarBicicletasPorTipo tipo path = do
    bicicletas <- obtenerBicicletas path
    putStrLn (mostrarBicicletasPorTipo' tipo bicicletas)

mostrarBicicletasPorTipo' :: String -> [Bicicleta] -> String
mostrarBicicletasPorTipo' tipo [] = ""
mostrarBicicletasPorTipo' tipo (x:xs)
    | tipo == (Bicicleta.tipo x) = (mostrarBicicleta x) ++
                                   (mostrarBicicletasPorTipo' tipo xs)
    | otherwise = mostrarBicicletasPorTipo' tipo xs

bicicletaPerteneceAParqueo :: String -> String -> String -> IO Bool
bicicletaPerteneceAParqueo codigo parqueo path = do
    bicicletas <- obtenerBicicletas path
    return (bicicletaPerteneceAParqueo' codigo parqueo bicicletas)

bicicletaPerteneceAParqueo' :: String -> String -> [Bicicleta] -> Bool
bicicletaPerteneceAParqueo' codigo parqueo [] = False
bicicletaPerteneceAParqueo' codigo parqueo (x:xs)
    | (codigo == (Bicicleta.codigo x)) && (parqueo == (Bicicleta.parqueo x)) = True
    | otherwise = bicicletaPerteneceAParqueo' codigo parqueo xs

cambiarParqueo :: Bicicleta -> String -> String -> IO ()
cambiarParqueo bicicleta parqueo path = do
    let bicicleta' = Bicicleta (Bicicleta.codigo bicicleta) (Bicicleta.tipo bicicleta) parqueo
    actualizarBicicleta bicicleta' path
