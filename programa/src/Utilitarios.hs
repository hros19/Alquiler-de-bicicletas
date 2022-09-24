
module Utilitarios(
    verificarEnteroValido,
    limpiarConsola,
    pausarConsola,
) where

{--------------------------------------------------------------------------------------------------
    Módulo: Utilitarios
    Descripción: Funciones utilitarias básicas que incluyen manejo de consola, operaciones I/0, etc.
    Autor(es):
        - Hansol Antay Rostrán
--------------------------------------------------------------------------------------------------}

-- Importación de módulos
import System.IO()
import System.Info
import System.Process
import Text.Read

mostrarMensaje :: String -> String -> String -> IO ()
mostrarMensaje titulo mensaje tipo = do
    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    putStrLn $ "[" ++ tipo ++ "] " ++ titulo ++ " [" ++ tipo ++ "]"
    putStrLn $ mensaje
    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

-- https://stackoverflow.com/questions/49367072/haskell-validate-integer-input-from-string-using-readmaybe
verificarEnteroValido :: String -> Bool
verificarEnteroValido strEntero
    | (readMaybe strEntero :: Maybe Int) == Nothing = False
    | otherwise = True

verificarFlotanteValido :: String -> Bool
verificarFlotanteValido strFlotante
    | (readMaybe strFlotante :: Maybe Float) == Nothing = False
    | otherwise = True

{-
    Función: limpiarConsola
    Descripción: Limpia la consola.
    Parámetros: --
    Retorno: --
-}
limpiarConsola :: IO ()
limpiarConsola = do
    case os of
        "linux" -> do
            _ <- system "clear"
            return ()
        "mingw32" -> do 
            _ <- system "cls"
            return ()
        _ -> do
            _ <- system "clear"
            return ()

{-
    Función: pausarConsola
    Descripción: Pausa la consola.
    Parámetros: --
    Retorno: --
-}
pausarConsola :: IO ()
pausarConsola = do
    putStrLn "[!] Presione una tecla para continuar... [!]"
    _ <- getChar
    limpiarConsola
    return ()
