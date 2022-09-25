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
import Usuario
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

mostrarMenuGestionComercio :: IO ()
mostrarMenuGestionComercio = do
    putStrLn "================== [GESTION DEL COMERCIO] =================="
    putStrLn "1. Mostrar información del comercio."
    putStrLn "2. Editar información del comercio."
    putStrLn "0. Volver al menú operativo."
    putStr "Digite la opción deseada: \n>"
    opcion <- getLine

    if (UT.verificarEnteroValido opcion) then do
        case (read opcion :: Int) of
            1 -> do
                UT.limpiarConsola
                putStrLn "============ [INFORMACION DEL COMERCIO] ============"
                Comercio.mostrarComercio
                putStrLn "===================================================="
                UT.pausarConsola
                mostrarMenuGestionComercio
            2 -> do
                editarDatosComercio
                mostrarMenuGestionComercio
            0 -> do
                UT.limpiarConsola
                mostrarMenuOperativo
            _ -> do
                UT.limpiarConsola
                let msj = "Debe digitar una opción válida."
                UT.mostrarMensaje "Error" msj "!!"
                UT.pausarConsola
                mostrarMenuGestionComercio
    else do
        UT.limpiarConsola
        let msj = "Debe digitar una opción válida."
        UT.mostrarMensaje "Error" msj "!!"
        UT.pausarConsola
        mostrarMenuGestionComercio

mostrarTodosLosParqueos :: IO ()
mostrarTodosLosParqueos = do
    UT.limpiarConsola
    putStrLn "\n============ [INFORMACION DE LOS PARQUEOS] ============"
    putStr "Ingrese la ubicacion del archivo (enter para el valor por defecto): "
    direccion <- getLine
    if (direccion == "") then do
        Parqueo.mostrarParqueos path_parqueo
        putStrLn "======================================================="
        UT.pausarConsola
        mostrarMenuGestionParqueos
    else do

    existeArchivo <- UT.verificarArchivoExistente direccion
    if (existeArchivo == True) then do
        Parqueo.mostrarParqueos direccion
        putStrLn "======================================================="
        UT.pausarConsola
        mostrarMenuGestionParqueos
    else do
        UT.limpiarConsola
        let msj = "El archivo no existe. Verifique la ruta ingresada."
        UT.mostrarMensaje "Error" msj "!!"
        UT.pausarConsola
        mostrarTodosLosParqueos

solicitarParqueosPorProvincia :: IO ()
solicitarParqueosPorProvincia = do
    UT.limpiarConsola
    putStrLn "\n============ [PARQUEOS POR PROVINCIA] ============"
    putStr "Ingrese la ubicacion del archivo (enter para el valor por defecto): "
    direccion <- getLine
    if (direccion == "") then do
        putStr "Ingrese la provincia: "
        provincia <- getLine
        if (provincia == "") then do
            UT.limpiarConsola
            let msj = "Debe digitar una provincia."
            UT.mostrarMensaje "Error" msj "!!"
            UT.pausarConsola
            solicitarParqueosPorProvincia
        else do
        let pertenece = UT.strPerteneceALista provincia ["SJ", "LI", "GU", "CA", "HE", "PU", "AL"]
        if (pertenece == False) then do
            UT.limpiarConsola
            let msj = "Debe digitar una provincia válida.\n" ++
                      "Las provincias válidas son: SJ, LI, GU, CA, HE, PU, AL."
            UT.mostrarMensaje "Error" msj "!!"
            UT.pausarConsola
            solicitarParqueosPorProvincia
        else do
            putStrLn "======================================================="
            Parqueo.mostrarParqueosPorProvincia path_parqueo provincia
            putStrLn "======================================================="
            UT.pausarConsola
            mostrarMenuGestionParqueos
    else do
    existeArchivo <- UT.verificarArchivoExistente direccion
    if (existeArchivo == True) then do
        putStr "Ingrese la provincia: "
        provincia <- getLine
        if (provincia == "") then do
            UT.limpiarConsola
            let msj = "Debe digitar una provincia."
            UT.mostrarMensaje "Error" msj "!!"
            UT.pausarConsola
            solicitarParqueosPorProvincia
        else do
        let pertenece = UT.strPerteneceALista provincia ["SJ", "LI", "GU", "CA", "HE", "PU", "AL"]
        if (pertenece == False) then do
            UT.limpiarConsola
            let msj = "Debe digitar una provincia válida.\n" ++
                      "Las provincias válidas son: SJ, LI, GU, CA, HE, PU, AL."
            UT.mostrarMensaje "Error" msj "!!"
            UT.pausarConsola
            solicitarParqueosPorProvincia
        else do
            putStrLn "======================================================="
            Parqueo.mostrarParqueosPorProvincia direccion provincia
            putStrLn "======================================================="
            UT.pausarConsola
            mostrarMenuGestionParqueos
    else do
        UT.limpiarConsola
        let msj = "El archivo no existe. Verifique la ruta ingresada."
        UT.mostrarMensaje "Error" msj "!!"
        UT.pausarConsola
        solicitarParqueosPorProvincia

solicitarBicisDeParqueo :: IO ()
solicitarBicisDeParqueo = do
    putStrLn "============ [BICICLETAS DE UN PARQUEO] ============"
    putStr "Ingrese la ubicacion del archivo (enter para el valor por defecto): "
    direccion <- getLine
    if (direccion == "") then do
        putStrLn "('#' para todos los parqueos/ 'transito' para bicicletas en transito)"
        putStr "Ingrese el nombre del parqueo: "
        nombre <- getLine
        if (nombre == "") then do
            UT.limpiarConsola
            let msj = "Debe digitar un nombre."
            UT.mostrarMensaje "Error" msj "!!"
            UT.pausarConsola
            solicitarBicisDeParqueo
        else if (nombre == "#") then do
            putStrLn "======================================================="
            Bicicleta.mostrarBicicletas path_bicicletas
            putStrLn "======================================================="
            UT.pausarConsola
            mostrarMenuGestionParqueos
        else if (nombre == "transito") then do
            putStrLn "===============[ BICICLETAS EN TRANSITO ]=============="
            Bicicleta.mostrarBicicletasEnTransito path_bicicletas
            putStrLn "======================================================="
            UT.pausarConsola
            mostrarMenuGestionParqueos
        else do
            putStrLn "======================================================="
            Bicicleta.mostrarBicicletasDeParqueo nombre path_bicicletas
            putStrLn "======================================================="
            UT.pausarConsola
            mostrarMenuGestionParqueos
    else do
    existeArchivo <- UT.verificarArchivoExistente direccion
    if (existeArchivo == True) then do
        putStr "Ingrese el nombre del parqueo ('#' para mostrar todas las bicis): "
        nombre <- getLine
        if (nombre == "") then do
            UT.limpiarConsola
            let msj = "Debe digitar un nombre."
            UT.mostrarMensaje "Error" msj "!!"
            UT.pausarConsola
            solicitarBicisDeParqueo
        else if (nombre == "#") then do
            putStrLn "======================================================="
            Bicicleta.mostrarBicicletas direccion
            putStrLn "======================================================="
            UT.pausarConsola
            mostrarMenuGestionParqueos
        else if (nombre == "transito") then do
            putStrLn "===============[ BICICLETAS EN TRANSITO ]=============="
            Bicicleta.mostrarBicicletasEnTransito direccion
            putStrLn "======================================================="
            UT.pausarConsola
            mostrarMenuGestionParqueos
        else do
            putStrLn "======================================================="
            Bicicleta.mostrarBicicletasDeParqueo nombre direccion
            putStrLn "======================================================="
            UT.pausarConsola
            mostrarMenuGestionParqueos
    else do
        UT.limpiarConsola
        let msj = "El archivo no existe. Verifique la ruta ingresada."
        UT.mostrarMensaje "Error" msj "!!"
        UT.pausarConsola
        solicitarBicisDeParqueo

mostrarMenuGestionParqueos :: IO ()
mostrarMenuGestionParqueos = do
    putStrLn "================== [GESTION DE LOS PARQUEOS] =================="
    putStrLn "1. Mostrar todos los parqueos."
    putStrLn "2. Mostrar parqueos por provincia."
    putStrLn "3. Mostrar bicicletas de parqueo. (OP.3)"
    putStrLn "0. Volver al menú operativo."
    putStr "Digite la opción deseada: \n>"
    opcion <- getLine

    if (UT.verificarEnteroValido opcion) then do
        case (read opcion :: Int) of
            1 -> mostrarTodosLosParqueos
            2 -> solicitarParqueosPorProvincia
            3 -> solicitarBicisDeParqueo
            0 -> do
                UT.limpiarConsola
                mostrarMenuOperativo
            _ -> do
                UT.limpiarConsola
                let msj = "Debe digitar una opción válida."
                UT.mostrarMensaje "Error" msj "!!"
                UT.pausarConsola
                mostrarMenuGestionParqueos
    else do
        UT.limpiarConsola
        let msj = "Debe digitar una opción válida."
        UT.mostrarMensaje "Error" msj "!!"
        UT.pausarConsola
        mostrarMenuGestionParqueos

solicitarTodasLasBicicletas :: IO ()
solicitarTodasLasBicicletas = do
    putStrLn "============ [TODAS LAS BICICLETAS] ============"
    putStr "Ingrese la ubicacion del archivo (enter para el valor por defecto): "
    direccion <- getLine
    if (direccion == "") then do
        putStrLn "======================================================="
        Bicicleta.mostrarBicicletas path_bicicletas
        putStrLn "======================================================="
        UT.pausarConsola
        mostrarMenuGestionBicicletas
    else do
        existeArchivo <- UT.verificarArchivoExistente direccion
        if (existeArchivo == True) then do
            putStrLn "======================================================="
            Bicicleta.mostrarBicicletas direccion
            putStrLn "======================================================="
            UT.pausarConsola
            mostrarMenuGestionBicicletas
        else do
            UT.limpiarConsola
            let msj = "El archivo no existe. Verifique la ruta ingresada."
            UT.mostrarMensaje "Error" msj "!!"
            UT.pausarConsola
            solicitarTodasLasBicicletas

solicitarBicicletasPorTipo :: IO ()
solicitarBicicletasPorTipo = do
    putStrLn "============ [BICICLETAS POR TIPO] ============"
    putStr "Ingrese la ubicacion del archivo (enter para el valor por defecto): "
    direccion <- getLine
    if (direccion == "") then do
        putStr "Ingrese el tipo de bicicleta (TR/AE): "
        tipo <- getLine
        let existe = UT.strPerteneceALista tipo ["TR", "AE"]
        if (existe == False) then do
            UT.limpiarConsola
            let msj = "Debe digitar un tipo válido. (TR o AE)"
            UT.mostrarMensaje "Error" msj "!!"
            UT.pausarConsola
            solicitarBicicletasPorTipo
        else do
            putStrLn "======================================================="
            Bicicleta.mostrarBicicletasPorTipo tipo path_bicicletas
            putStrLn "======================================================="
            UT.pausarConsola
            mostrarMenuGestionBicicletas
    else do
        existeArchivo <- UT.verificarArchivoExistente direccion
        if (existeArchivo == True) then do
            putStr "Ingrese el tipo de bicicleta (TR/AE): "
            tipo <- getLine
            let existe = UT.strPerteneceALista tipo ["TR", "AE"]
            if (existe == False) then do
                UT.limpiarConsola
                let msj = "Debe digitar un tipo válido. (TR o AE)"
                UT.mostrarMensaje "Error" msj "!!"
                UT.pausarConsola
                solicitarBicicletasPorTipo
            else do
                putStrLn "======================================================="
                Bicicleta.mostrarBicicletasPorTipo tipo direccion
                putStrLn "======================================================="
                UT.pausarConsola
                mostrarMenuGestionBicicletas
        else do
            UT.limpiarConsola
            let msj = "El archivo no existe. Verifique la ruta ingresada."
            UT.mostrarMensaje "Error" msj "!!"
            UT.pausarConsola
            solicitarBicicletasPorTipo

mostrarMenuGestionBicicletas :: IO ()
mostrarMenuGestionBicicletas = do
    putStrLn "================== [GESTION DE BICICLETAS] =================="
    putStrLn "1. Mostrar todas las bicicletas."
    putStrLn "2. Mostrar bicicletas por tipo."
    putStrLn "0. Volver al menú operativo."
    putStr "Digite la opción deseada: \n>"
    opcion <- getLine

    if (UT.verificarEnteroValido opcion) then do
        case (read opcion :: Int) of
            1 -> do
                UT.limpiarConsola
                solicitarTodasLasBicicletas
            2 -> do
                UT.limpiarConsola
                solicitarBicicletasPorTipo
            0 -> do
                UT.limpiarConsola
                mostrarMenuOperativo
            _ -> do
                UT.limpiarConsola
                let msj = "Debe digitar una opción válida."
                UT.mostrarMensaje "Error" msj "!!"
                UT.pausarConsola
                mostrarMenuGestionBicicletas
    else do
        UT.limpiarConsola
        let msj = "Debe digitar una opción válida."
        UT.mostrarMensaje "Error" msj "!!"
        UT.pausarConsola
        mostrarMenuGestionBicicletas

mostrarUsuariosRegistrados :: IO ()
mostrarUsuariosRegistrados = do
    putStrLn "============ [USUARIOS REGISTRADOS] ============"
    putStr "Ingrese la ubicacion del archivo (enter para el valor por defecto): "
    direccion <- getLine
    if (direccion == "") then do
        putStrLn "======================================================="
        Usuario.mostrarUsuarios path_usuarios
        putStrLn "======================================================="
        UT.pausarConsola
        mostrarMenuOperativo
    else do
        existeArchivo <- UT.verificarArchivoExistente direccion
        if (existeArchivo == True) then do
            putStrLn "======================================================="
            Usuario.mostrarUsuarios direccion
            putStrLn "======================================================="
            UT.pausarConsola
            mostrarMenuOperativo
        else do
            UT.limpiarConsola
            let msj = "El archivo no existe. Verifique la ruta ingresada."
            UT.mostrarMensaje "Error" msj "!!"
            UT.pausarConsola
            mostrarUsuariosRegistrados

mostrarMenuOperativo :: IO ()
mostrarMenuOperativo = do
    putStrLn "============ [MENU OPERATIVO] ============"
    putStrLn "1. Gestión del comercio (info. comercial)"
    putStrLn "2. Gestión de los parqueos"
    putStrLn "3. Gestión de las bicicletas"
    putStrLn "4. Mostrar usuarios registrados"
    putStrLn "5. Estadísticas... (pendiente)"
    putStrLn "0. Volver al menú principal"
    putStr "Digite la opcion deseada: \n>"
    opcion <- getLine

    if (UT.verificarEnteroValido opcion) then do
        case (read opcion :: Int) of
            1 -> do
                UT.limpiarConsola
                mostrarMenuGestionComercio
            2 -> do
                UT.limpiarConsola
                mostrarMenuGestionParqueos
            3 -> do
                UT.limpiarConsola
                mostrarMenuGestionBicicletas
            4 -> do
                UT.limpiarConsola
                mostrarUsuariosRegistrados
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
