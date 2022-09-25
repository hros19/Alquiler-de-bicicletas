
module Utilitarios where

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
import System.Directory
import Text.Read

{-
    Función: mostrarMensaje
    Descripción: Muestra un mensaje en la consola.
    Parámetros:
        - titulo: Título del mensaje.
        - mensaje: Mensaje a mostrar.
        - tipo: Símbolo del mensaje. (!!, ??, **, etc...)
    Retorno: Ninguno.
-}
mostrarMensaje :: String -> String -> String -> IO ()
mostrarMensaje titulo mensaje tipo = do
    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    putStrLn $ "[" ++ tipo ++ "] " ++ titulo ++ " [" ++ tipo ++ "]"
    putStrLn $ mensaje
    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

{-
    Función: verificarEnteroValido
    Descripción: Verifica si un String es un entero válido.
    Parámetros:
        - String: El String a verificar.
    Retorno:
        - Bool: True si el String es un entero válido, False en caso contrario.
    Source: https://stackoverflow.com/questions/49367072/haskell-validate-integer-input-from-string-using-readmaybe
-}
verificarEnteroValido :: String -> Bool
verificarEnteroValido strEntero
    | (readMaybe strEntero :: Maybe Int) == Nothing = False
    | otherwise = True

{-
    Función: verificarFlotanteValido
    Descripción: Verifica si un String es un flotante (double) válido.
    Parámetros:
        - String: El String a verificar.
    Retorno:
        - Bool: True si el String es un flotante (double) válido, False en caso contrario.
    Source: https://stackoverflow.com/questions/49367072/haskell-validate-integer-input-from-string-using-readmaybe
-}
verificarFlotanteValido :: String -> Bool
verificarFlotanteValido strFlotante
    | (readMaybe strFlotante :: Maybe Double) == Nothing = False
    | otherwise = True

{-
    Función: verificarNumeroNegativo
    Descripción: Verifica si un String es un número double positivo.
    Parámetros:
        - String: El String a verificar.
    Retorno:
        - Bool: True si el String es un número double positivo, False en caso contrario.
-}
verificarNumeroPositivo :: String -> Bool
verificarNumeroPositivo strNumero
    | ((readMaybe strNumero :: Maybe Double) == Nothing) = False
    | (read strNumero :: Double) < 0 = False
    | otherwise = True

verificarArchivoExistente :: String -> IO Bool
verificarArchivoExistente ruta = do
    existe <- (doesFileExist ruta)
    return (existe :: Bool)

strPerteneceALista :: String -> [String] -> Bool
strPerteneceALista str lista = do
    if (length lista) == 0 then
        False
    else if (str == (head lista)) then
        True
    else
        strPerteneceALista str (tail lista)

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
