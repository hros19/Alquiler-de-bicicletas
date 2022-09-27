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

{-
    Función: obtenerUsuarios
    Descripción: Obtiene la lista de usuarios de la aplicación.
    Objetivo: Obtener la lista de usuarios de la aplicación.
    Parámetros:
        - path: Ruta del archivo que contiene la lista de usuarios.
    Retorno: Lista de usuarios.
    Restricciones: --
-}
obtenerUsuarios :: String -> IO [Usuario]
obtenerUsuarios path = do
    contenido <- BS.readFile path
    let usuarios = decode contenido :: Maybe [Usuario]
    return (case usuarios of
        Just usuarios -> usuarios
        Nothing -> [])

{-
    Función: obtenerUsuario
    Descripción: Obtiene un usuario de la aplicación.
    Parámetros:
        - path: Ruta del archivo que contiene la lista de usuarios.
        - cedula: Cédula del usuario a obtener.
    Retorno: El usuario con la cédula especificada.
    Restricciones: --
-}
obtenerUsuario :: String -> String -> IO Usuario
obtenerUsuario path cedula = do
    usuarios <- obtenerUsuarios path
    return (obtenerUsuario' usuarios cedula)

{-
    Función: obtenerUsuario'
    Descripción: Obtiene un usuario de la aplicación.
    Parámetros:
        - usuarios: Lista de usuarios.
        - cedula: Cédula del usuario a obtener.
    Retorno:
        - El usuario con la cédula especificada.
        - Usuario vacío si no se encuentra el usuario.
    Restricciones: --
-}
obtenerUsuario' :: [Usuario] -> String -> Usuario
obtenerUsuario' [] ced = Usuario "" ""
obtenerUsuario' (x:xs) ced
    | ced == (cedula x) = x
    | otherwise = obtenerUsuario' xs ced

{-
    Función: mostrarUsuario
    Descripción: Pasa la información de un usuario a un String.
    Objetivo: Mostrar la información de un usuario.
    Parámetros:
        - usuario: Usuario a mostrar.
    Retorno:
        - String con la información del usuario.
    Restricciones: --
-}
mostrarUsuario :: Usuario -> String
mostrarUsuario usuario = "Cédula: " ++ (Usuario.cedula usuario) ++ "\n" ++
                         "Nombre: " ++ (Usuario.nombre usuario) ++ "\n" ++
                         "-----------------------------------------------\n"

{-
    Función: mostrarUsuarios
    Descripción: Muestra todos los usuarios en pantalla de una ruta especificada.
    Objetivo: Mostrar todos los usuarios de una ruta especificada.
    Parámetros:
        - path: Ruta del archivo que contiene la lista de usuarios.
    Retorno: --
    Restricciones: --
-}
mostrarUsuarios :: String -> IO ()
mostrarUsuarios path = do
    usuarios <- obtenerUsuarios path
    putStrLn (mostrarUsuarios' usuarios)

{-
    Función: mostrarUsuarios'
    Descripción: Muestra todos los usuarios en pantalla de una lista especificada.
    Parámetros:
        - usuarios: Lista de usuarios.
    Retorno:
        - String con la información de todos los usuarios.
    Restricciones: --
-}
mostrarUsuarios' :: [Usuario] -> String
mostrarUsuarios' [] = ""
mostrarUsuarios' (x:xs) = (mostrarUsuario x) ++ (mostrarUsuarios' xs)

{-
    Función: existeCedula
    Descripción: Verifica si una cédula existe en una ruta con usuarios.
    Parámetros:
        - path: Ruta del archivo que contiene la lista de usuarios.
        - cedula: Cédula a verificar.
    Retorno:
        - True si la cédula existe.
        - False si la cédula no existe.
    Restricciones: --
-}
existeCedula :: String -> String -> IO Bool
existeCedula cedula path = do
    usuarios <- obtenerUsuarios path
    return (existeCedula' cedula usuarios)

{-
    Función: existeCedula'
    Descripción: Verifica si una cédula existe en una lista de usuarios.
    Parámetros:
        - cedula: Cédula a verificar.
        - usuarios: Lista de usuarios.
    Retorno:
        - True si la cédula existe.
        - False si la cédula no existe.
    Restricciones: --
-}
existeCedula' :: String -> [Usuario] -> Bool
existeCedula' _ [] = False
existeCedula' cedula (x:xs) = 
    if (Usuario.cedula x) == cedula then True 
    else existeCedula' cedula xs
