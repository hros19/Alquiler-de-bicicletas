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
Modelo: Bicicletas
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

obtenerBicicletas :: IO [Bicicleta]
obtenerBicicletas = do
    contenido <- BS.readFile path_bicicletas
    let bicicletas = decode contenido :: Maybe [Bicicleta]
    case bicicletas of
        Nothing -> return []
        Just bicicletas -> return bicicletas

obtenerBicicleta :: String -> IO (Maybe Bicicleta)
obtenerBicicleta codigo = do
    bicicletas <- obtenerBicicletas
    return (obtenerBicicleta' codigo bicicletas)

obtenerBicicleta' :: String -> [Bicicleta] -> Maybe Bicicleta
obtenerBicicleta' codigo [] = Nothing
obtenerBicicleta' codigo (x:xs)
    | codigo == (Bicicleta.codigo x) = Just x
    | otherwise = obtenerBicicleta' codigo xs

eliminarBicicleta :: String -> IO ()
eliminarBicicleta codigo = do
    bicicletas <- obtenerBicicletas
    let bicicletas' = eliminarBicicleta' codigo bicicletas
    BS.writeFile path_bicicletas (encodePretty bicicletas')

eliminarBicicleta' :: String -> [Bicicleta] -> [Bicicleta]
eliminarBicicleta' codigo [] = []
eliminarBicicleta' codigo (x:xs)
    | codigo == (Bicicleta.codigo x) = xs
    | otherwise = x : eliminarBicicleta' codigo xs

actualizarBicicleta :: Bicicleta -> IO ()
actualizarBicicleta bicicleta = do
    bicicletas <- obtenerBicicletas
    let bicicletas' = actualizarBicicleta' bicicleta bicicletas
    BS.writeFile path_bicicletas (encodePretty bicicletas')

actualizarBicicleta' :: Bicicleta -> [Bicicleta] -> [Bicicleta]
actualizarBicicleta' bicicleta [] = []
actualizarBicicleta' bicicleta (x:xs)
    | (Bicicleta.codigo bicicleta) == (Bicicleta.codigo x) = bicicleta : xs
    | otherwise = x : actualizarBicicleta' bicicleta xs

mostrarBicicleta :: Bicicleta -> String
mostrarBicicleta bicicleta = "Código: " ++ (Bicicleta.codigo bicicleta) ++ "\n" ++
                             "Tipo: " ++ (Bicicleta.tipo bicicleta) ++ "\n" ++
                             "Parqueo: " ++ (Bicicleta.parqueo bicicleta)

mostrarBicicletas :: IO ()
mostrarBicicletas = do
    bicicletas <- obtenerBicicletas
    putStrLn (mostrarBicicletas' bicicletas)

mostrarBicicletas' :: [Bicicleta] -> String
mostrarBicicletas' [] = ""
mostrarBicicletas' (x:xs) = (mostrarBicicleta x) ++ 
                            "\n----------------------------------\n" ++
                            (mostrarBicicletas' xs)


