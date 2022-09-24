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
import Bicicleta
import Parqueo
import qualified Utilitarios as UT

editarDatosComercio :: IO ()
editarDatosComercio = do
    UT.limpiarConsola
    putStrLn "============ [INFORMACION ACTUAL DEL COMERCIO] ============"
    Comercio.mostrarComercio
    putStrLn "==========================================================="
    putStrLn "NOTA: Si desea cancelar, presione (enter) en cualquier momento.\n"
    putStrLn "Ingrese el nuevo nombre del comercio: "
    nombre <- getLine
    if (nombre == "") then do
        UT.limpiarConsola
        let msj = "No se ha realizado ningún cambio."
        UT.mostrarMensaje "Info" msj "**"
        UT.pausarConsola
        return ()
    else do
    
    putStrLn "Ingrese la nueva dirección web: "
    web <- getLine
    if (web == "") then do
        UT.limpiarConsola
        let msj = "No se ha realizado ningún cambio."
        UT.mostrarMensaje "Info" msj "**"
        UT.pausarConsola
        return ()
    else do
    
    putStrLn "Ingrese la nueva info. de contacto: "
    contacto <- getLine
    if (contacto == "") then do
        UT.limpiarConsola
        let msj = "No se ha realizado ningún cambio."
        UT.mostrarMensaje "Info" msj "**"
        UT.pausarConsola
        return ()
    else do

    -- Tarifas (numérico / double)
    putStrLn "Ingrese la nueva tarifa por km pedal: "
    tarifaKmPedal <- getLine
    if (tarifaKmPedal == "") then do
        UT.limpiarConsola
        let msj = "No se ha realizado ningún cambio."
        UT.mostrarMensaje "Info" msj "**"
        UT.pausarConsola
        return ()
    else do

    putStrLn "Ingrese la nueva tarifa por km eléctrico: "
    tarifaKmElectrico <- getLine
    if (tarifaKmElectrico == "") then do
        UT.limpiarConsola
        let msj = "No se ha realizado ningún cambio."
        UT.mostrarMensaje "Info" msj "**"
        UT.pausarConsola
        return ()
    else do

    if (UT.verificarNumeroPositivo tarifaKmPedal == False) then do
        UT.limpiarConsola
        let msj = "Debe digitar un número flotante positivo para el valor " ++
                  "de la tarifa por km pedal.\n" ++
                  "Digitó: `" ++ tarifaKmPedal ++ "`\n"
        UT.mostrarMensaje "Error" msj "!!"
        UT.pausarConsola
        editarDatosComercio
    else if (UT.verificarNumeroPositivo tarifaKmElectrico == False) then do
        UT.limpiarConsola
        let msj = "Debe digitar un número flotante positivo para el valor " ++
                  "de la tarifa por km eléctrico.\n" ++
                  "Digitó: `" ++ tarifaKmElectrico ++ "`\n"
        UT.mostrarMensaje "Error" msj "!!"
        UT.pausarConsola
        editarDatosComercio
    else do
        let tarifaKmPedal' = read tarifaKmPedal :: Double
        let tarifaKmElectrico' = read tarifaKmElectrico :: Double
        let comercio = Comercio nombre web contacto tarifaKmPedal' tarifaKmElectrico'
        UT.limpiarConsola
        let msj = "Los datos del comercio han sido actualizados."
        UT.mostrarMensaje "Info" msj "**"
        UT.pausarConsola
        Comercio.guardarComercio comercio
        return ()

mostrarMenuOperativo :: IO ()
mostrarMenuOperativo = do
    putStrLn "============ [MENU OPERATIVO] ============"
    putStrLn "1. Mostrar información del comercio"
    putStrLn "2. Editar información del comercio"
    putStrLn "3. Cargar y mostrar parqueos"
    putStrLn "4. Cargar y mostrar bicicletas"
    putStrLn "0. Volver al menú principal"
    putStr "Digite la opcion deseada: \n>"
    opcion <- getLine

    if (UT.verificarEnteroValido opcion) then do
        case (read opcion :: Int) of
            1 -> do
                UT.limpiarConsola
                putStrLn "============ [INFORMACION DEL COMERCIO] ============"
                Comercio.mostrarComercio
                putStrLn "===================================================="
                UT.pausarConsola
                mostrarMenuOperativo
            2 -> do
                editarDatosComercio
                mostrarMenuOperativo
            3 -> do
                UT.limpiarConsola
                putStrLn "\n============ [INFORMACION DE LOS PARQUEOS] ============"
                putStr "Ingrese la ubicacion del archivo: "
                direccion <- getLine
                Parqueo.mostrarParqueos direccion                
                UT.pausarConsola
                mostrarMenuOperativo
            4 -> do
                UT.limpiarConsola
                putStrLn "============ [BICICLETAS] ============"
                Bicicleta.mostrarBicicletas
                putStrLn "======================================"
                UT.pausarConsola
                mostrarMenuOperativo
            0 -> putStrLn "Saliendo..."
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
    putStrLn "0. Sair"
    putStr "Digite la opcion deseada: \n>"
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
