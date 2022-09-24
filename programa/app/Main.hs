module Main where

{--------------------------------------------------------------------------------------------------
    Módulo: Main
    Descripción: Módulo principal del programa (donde se ejecuta el programa).
    Fecha de creación: 2022-09-24 (24 de setiembre de 2022).
    Autor(es):
        - Hansol Antay Rostrán
        - Alexander Sánchez Céspedes
--------------------------------------------------------------------------------------------------}


import Data.Aeson
import Data.Aeson.Encode.Pretty
import GHC.Generics
import qualified Data.ByteString.Lazy as BS

-- Importaciones locales
import Comercio
import qualified Utilitarios as UT

mostrarMenuOperativo :: IO ()
mostrarMenuOperativo = do
    putStrLn "============ [MENU OPERATIVO] ============"
    putStrLn "1. Mostrar información del comercio"
    putStrLn "2. Editar información del comercio"
    putStrLn "[>>] Digite la opcion deseada:"
    opcion <- getLine

    if (UT.verificarEnteroValido opcion) then do
        case (read opcion :: Int) of
            1 -> do
                UT.limpiarConsola
                mostrarMenuOperativo
            2 -> putStrLn "...."
            3 -> putStrLn "Saliendo..."
            _ -> do
                UT.limpiarConsola
                let msj = "Debe digitar una opcion valida." ++
                          "Digito: `" ++ opcion ++ "`" ++
                          "Opciones validas: 1, 2, 3"
                UT.mostrarMensaje "Error" msj "!!"
                UT.pausarConsola
                mostrarMenuPrincipal
    else do
        UT.limpiarConsola
        let msj = "Debe digitar un número entero válido.\n" ++
                  "Digito: `" ++ opcion ++ "`\n" ++
                  "Valores validos: 1, 2, 3"
        UT.mostrarMensaje "Error" msj "!!"
        UT.pausarConsola
        mostrarMenuPrincipal

mostrarMenuPrincipal :: IO ()
mostrarMenuPrincipal = do
    putStrLn "============ [MENU PRINCIPAL] ============"
    putStrLn "1. Opciones operativas"
    putStrLn "2. Opciones generales"
    putStrLn "3. Sair"
    putStrLn "[>>] Digite la opcion deseada:"
    opcion <- getLine

    if (UT.verificarEnteroValido opcion) then do
        case (read opcion :: Int) of
            1 -> do
                UT.limpiarConsola
                mostrarMenuOperativo
            2 -> putStrLn "...."
            3 -> putStrLn "Saliendo..."
            _ -> do
                UT.limpiarConsola
                let msj = "Debe digitar una opcion valida." ++
                          "Digito: `" ++ opcion ++ "`" ++
                          "Opciones validas: 1, 2, 3"
                UT.mostrarMensaje "Error" msj "!!"
                UT.pausarConsola
                mostrarMenuPrincipal
    else do
        UT.limpiarConsola
        let msj = "Debe digitar un número entero válido.\n" ++
                  "Digito: `" ++ opcion ++ "`\n" ++
                  "Valores validos: 1, 2, 3"
        UT.mostrarMensaje "Error" msj "!!"
        UT.pausarConsola
        mostrarMenuPrincipal

main :: IO ()
main = do
    UT.limpiarConsola
    mostrarMenuPrincipal
