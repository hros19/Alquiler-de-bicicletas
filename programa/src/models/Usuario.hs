{-# LANGUAGE DeriveGeneric #-}

module Usuario where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import GHC.Generics
import qualified Data.ByteString.Lazy as BS

-- Variables globales

path_usuarios :: FilePath
path_usuarios = "./src/data/usuarios.json"

{--------------------------------------------------------------------------------------------------
Modelo: Usuario
Versión: 1.0 (24 de setiembre del 2022)
Autor: Hansol Antay Rostrán (https://github.com/hros19)
---------------------------------------------------------------------------------------------------
Descripción y propósito: 
    Representará los usuarios de la app. Su propósito es poder almacenar y manipular la información de
    los usuarios de la aplicación.
Atributos:
    cedula: Cédula de la persona.
    nombre: Nombre completo de la persona.
--------------------------------------------------------------------------------------------------}

data Usuario = Usuario {
    cedula :: String,
    nombre :: String
} deriving (Show, Generic)

instance FromJSON Usuario
instance ToJSON Usuario

obtenerUsuarios :: String -> IO [Usuario]
obtenerUsuarios path = do
    contenido <- BS.readFile path
    let usuarios = decode contenido :: Maybe [Usuario]
    return (case usuarios of
        Just usuarios -> usuarios
        Nothing -> [])

mostrarUsuario :: Usuario -> String
mostrarUsuario usuario = "Cédula: " ++ (Usuario.cedula usuario) ++ "\n" ++
                         "Nombre: " ++ (Usuario.nombre usuario) ++ "\n" ++
                         "--------------------------------------------\n"

mostrarUsuarios :: String -> IO ()
mostrarUsuarios path = do
    usuarios <- obtenerUsuarios path
    putStrLn (mostrarUsuarios' usuarios)

mostrarUsuarios' :: [Usuario] -> String
mostrarUsuarios' [] = ""
mostrarUsuarios' (x:xs) = (mostrarUsuario x) ++ (mostrarUsuarios' xs)

existeCedula :: String -> String -> IO Bool
existeCedula cedula path = do
    usuarios <- obtenerUsuarios path
    return (existeCedula' cedula usuarios)

existeCedula' :: String -> [Usuario] -> Bool
existeCedula' _ [] = False
existeCedula' cedula (x:xs) = 
    if (Usuario.cedula x) == cedula then True 
    else existeCedula' cedula xs
