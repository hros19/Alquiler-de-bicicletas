module Main where

{--------------------------------------------------------------------------------------------------
    Módulo: Main
    Descripción: Módulo principal del programa (donde se ejecuta el programa).
    Fecha de creación: 2022-09-24 (24 de setiembre de 2022).
    Autor(es):
        - Hansol Antay Rostrán
        - Alexander Sánchez Céspedes
--------------------------------------------------------------------------------------------------}

import Data.Maybe
import Data.Aeson
import Data.Aeson.Encode.Pretty
import GHC.Generics
import Data.Tuple.Select
import qualified Data.ByteString.Lazy as BS

-- Importaciones locales
import Comercio
import Bicicleta
import Parqueo
import Usuario
import Factura
import Recibo
import qualified Utilitarios as UT

{-
    Función: editarDatosComercio
    Descripción: Función que permite editar los datos del comercio.
    Objetivo: Permitir al usuario editar datos básicos del comercio.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
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

{-
    Función: mostrarMenuGestionComercio
    Descripción: muestra el menú de gestión del comercio.
    Objetivo: mostrar el usuario las opciones disponibles para gestionar el comercio.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
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

{-
    Función: mostrarTodosLosParqueos
    Descripción: muestra todos los parqueos registrados en el sistema.
    Objetivo: el objetivo es pedirle al usuario el path del archivo con todos los
              parqueos registrados en el sistema y mostrarlos en pantalla.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
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

{-
    Función: solicitarParqueosPorProvincia
    Descripción: muestra todos los parqueos registrados en el sistema de una provincia.
    Objetivo: el objetivo es pedirle al usuario el path del archivo, la provincia y
              mostrar los parqueos de la provincia en pantalla.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
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

{-
    Función: solicitarBicisDeParqueo
    Descripción: solicita todas las bicicletas de un parqueo.
    Objetivo: el objetivo es pedirle al usuario el path del archivo de bicicletas,
              el nombre del parqueo y mostrar las bicicletas del parqueo en pantalla.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
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

{-
    Función: mostrarMenuGestionParqueos
    Descripción: muestra el menú de gestión de parqueos.
    Objetivo: pedir al usuario que seleccione una opción del menú para gestionar parqueos.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
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

{-
    Función: solicitarTodasLasBicicletas
    Descripción: solicita todas las bicicletas del sistema para mostrarlas.
    Objetivo: el objetivo es pedirle una ubicación al usuario del archivo de bicicletas
              y mostrar todas las bicicletas en pantalla.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
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

{-
    Función: solicitarBicisDeParqueo
    Descripción: solicita todas las bicicletas de un parqueo.
    Objetivo: el objetivo es pedirle al usuario el path del archivo de bicicletas,
              el nombre del parqueo y mostrar las bicicletas del parqueo en pantalla.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
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

{-
    Función: mostrarMenuGestionBicicletas
    Descripción: muestra un menú para la gestión de bicicletas.
    Objetivo: pide una opción al usuario para que pueda gestionar las bicicletas.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
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

{-
    Función: mostrarUsuariosRegistrados
    Descripción: muestra todos los usuarios registrados en un archivo.
    Objetivo: el objetivo es pedirle al usuario el path del archivo de usuarios y
              mostrar los usuarios registrados en pantalla.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
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

{-
    Función: mostrarMenuEstadisticas
    Descripción: muestra el menu de las estadisticas.
    Objetivo: ofreceler un menu de opciones al usuario para que pueda visualizar
              las estadisticas.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
mostrarMenuEstadisticas :: IO ()
mostrarMenuEstadisticas = do
    putStrLn "================== [ESTADISTICAS] =================="
    putStrLn "1. Top 5 usuarios con más viajes."
    putStrLn "2. Top 5 parqueos con más viajes."
    putStrLn "3. Top 3 bicicletas con más kilometros recorridos."
    putStrLn "4. Resumen"
    putStrLn "0. Volver al menú opciones operativas."
    putStr "Digite la opción deseada: \n>"
    opcion <- getLine

    if (UT.verificarEnteroValido opcion) then do
        case (read opcion :: Int) of
            1 -> do
                UT.limpiarConsola
                Factura.mostrarTopCincoUsuariosConMasViajes
                UT.pausarConsola
                mostrarMenuEstadisticas
            2 -> do
                UT.limpiarConsola
                Factura.mostrarTopCincoParqueosConMasViajes
                UT.pausarConsola
                mostrarMenuEstadisticas
            3 -> do
                UT.limpiarConsola
                Factura.mostrarTopTresBicisConMasDistancia
                UT.pausarConsola
                mostrarMenuEstadisticas
            4 -> do
                UT.limpiarConsola
                putStrLn "Resumen general:"
                Factura.mostrarResumen
                UT.pausarConsola
                mostrarMenuEstadisticas
            0 -> do
                UT.limpiarConsola
                mostrarMenuOperativo
            _ -> do
                UT.limpiarConsola
                let msj = "Debe digitar una opción válida."
                UT.mostrarMensaje "Error" msj "!!"
                UT.pausarConsola
                mostrarMenuEstadisticas
    else do
        UT.limpiarConsola
        let msj = "Debe digitar una opción válida."
        UT.mostrarMensaje "Error" msj "!!"
        UT.pausarConsola
        mostrarMenuEstadisticas

{-
    Función: mostrarMenuOperativo
    Descripción: muestra el menu de opciones operativas.
    Objetivo: que el usuario pueda seleccionar una opcion para navegar entre las opciones operativas.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
mostrarMenuOperativo :: IO ()
mostrarMenuOperativo = do
    putStrLn "============ [MENU OPERATIVO] ============"
    putStrLn "1. Gestión del comercio (info. comercial)"
    putStrLn "2. Gestión de los parqueos"
    putStrLn "3. Gestión de las bicicletas"
    putStrLn "4. Mostrar usuarios registrados"
    putStrLn "5. Estadísticas"
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
            5 -> do
                UT.limpiarConsola
                mostrarMenuEstadisticas
            0 -> do
                UT.limpiarConsola
                mostrarMenuPrincipal
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

{-
    Función: solicitarParqueoMasCercano
    Descripción: solicita el parqueo que se encuentre más cercano.
    Objetivo: pedir al usuario un punto (x,y) para luego mostrarle el parqueo más cercano.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
solicitarParqueoMasCercano :: IO ()
solicitarParqueoMasCercano = do
    putStrLn "============ [PARQUEO MAS CERCANO] ============"
    putStr "Ingrese la ubicacion del archivo (enter para el valor por defecto): "
    direccion <- getLine
    if (direccion == "") then do
        putStr "Ingrese la coordenada en x: "
        x <- getLine
        if (UT.verificarNumeroPositivo x) then do
            putStr "Ingrese la coordenada en y: "
            y <- getLine
            if (UT.verificarNumeroPositivo y) then do
                UT.limpiarConsola
                parqueos <- Parqueo.obtenerParqueos "./src/data/parqueo.json"
                let distancias = Parqueo.obtenerDistancias parqueos (read x :: Double) (read y :: Double)
                let menorDistancia = Parqueo.obtenerMenorDistancia distancias
                let nombreParqueo = sel2 menorDistancia
                let distancia = sel1 menorDistancia
                let msj = "El parqueo mas cercano es: " ++ nombreParqueo ++ " a " ++ (show distancia) ++ " metros."
                UT.mostrarMensaje "Resultado" msj "!!"
                UT.pausarConsola
                mostrarMenuGeneral
            else do
                UT.limpiarConsola
                let msj = "Debe digitar un número positivo."
                UT.mostrarMensaje "Error" msj "!!"
                UT.pausarConsola
                solicitarParqueoMasCercano
        else do
            UT.limpiarConsola
            let msj = "Debe digitar un número positivo."
            UT.mostrarMensaje "Error" msj "!!"
            UT.pausarConsola
            solicitarParqueoMasCercano
    else do
        existeArchivo <- UT.verificarArchivoExistente direccion
        if (existeArchivo == True) then do
            putStr "Ingrese la coordenada en x: "
            x <- getLine
            if (UT.verificarNumeroPositivo x) then do
                putStr "Ingrese la coordenada en y: "
                y <- getLine
                if (UT.verificarNumeroPositivo y) then do
                    UT.limpiarConsola
                    parqueos <- Parqueo.obtenerParqueos "./src/data/parqueo.json"
                    let distancias = Parqueo.obtenerDistancias parqueos (read x :: Double) (read y :: Double)
                    let menorDistancia = Parqueo.obtenerMenorDistancia distancias
                    let nombreParqueo = sel2 menorDistancia
                    let distancia = sel1 menorDistancia
                    let msj = "El parqueo mas cercano es: " ++ nombreParqueo ++ " a " ++ (show distancia) ++ " metros."
                    UT.mostrarMensaje "Resultado" msj "!!"
                    UT.pausarConsola
                    mostrarMenuGeneral
                else do
                    UT.limpiarConsola
                    let msj = "Debe digitar un número positivo."
                    UT.mostrarMensaje "Error" msj "!!"
                    UT.pausarConsola
                    solicitarParqueoMasCercano
            else do
                UT.limpiarConsola
                let msj = "Debe digitar un número positivo."
                UT.mostrarMensaje "Error" msj "!!"
                UT.pausarConsola
                solicitarParqueoMasCercano
        else do
            UT.limpiarConsola
            let msj = "El archivo no existe. Verifique la ruta ingresada."
            UT.mostrarMensaje "Error" msj "!!"
            UT.pausarConsola
            solicitarParqueoMasCercano

{-
    Función: obtenerIdFacturaNuevo
    Descripción: permite obtener un nuevo int aleatorio.
    Objetivo: poder generar identificadores de las facturas de forma pseudoaleatoria.
    Parámetros: --
    Retorno: Int -> identificador de la factura (pseudoaleatorio).
    Restricciones: --
-}
obtenerIdFacturaNuevo :: IO Int
obtenerIdFacturaNuevo = do
    nuevoId <- UT.generarIdFactura
    existeId <- Factura.existeFactura nuevoId
    if (existeId == True) then do
        obtenerIdFacturaNuevo
    else do
        return nuevoId

{-
    Función: solicitarAlquilerBicicleta
    Descripción: solicita un nuevo alquiler de bicicletas.
    Objetivo: permite al usuario alquilar una bicicleta que se encuentre desocupada (no "En tránsito"), a un
              usuario a partir de su cédula, y parqueos para definir el punto de inicio y fin del alquiler.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
solicitarAlquilerBicicleta :: IO ()
solicitarAlquilerBicicleta = do
    putStrLn "============ [ALQUILER DE BICICLETA] ============"
    putStr "Ingrese la ubicacion del archivo de las bicicletas (enter para el valor por defecto): "
    direccion <- getLine
    if (direccion == "") then do
        putStr "Ingrese la cedula del usuario: "
        cedula <- getLine
        bExisteCed <- Usuario.existeCedula cedula "./src/data/usuarios.json"
        if (bExisteCed == True) then do
        -- El usuario existe

        putStr "Ingrese el parqueo de salida: "
        parqueoSalida <- getLine
        putStr "Ingrese el parqueo de llegada: "
        parqueoLlegada <- getLine

        bExisteParqSal <- Parqueo.existeParqueo "./src/data/parqueo.json" parqueoSalida
        bExisteParqLleg <- Parqueo.existeParqueo "./src/data/parqueo.json" parqueoLlegada

        if (bExisteParqSal == False || bExisteParqLleg == False) then do
            UT.limpiarConsola
            let msj = "Debe ingresar parqueos existentes."
            UT.mostrarMensaje "Error" msj "!!"
            UT.pausarConsola
            solicitarAlquilerBicicleta
        else do

        if (parqueoSalida == "En tránsito" || parqueoLlegada == "En tránsito") then do
            UT.limpiarConsola
            let msj = "No se puede alquilar una bicicleta en tránsito."
            UT.mostrarMensaje "Error" msj "!!"
            UT.pausarConsola
            solicitarAlquilerBicicleta
        else do

        if (parqueoSalida == parqueoLlegada) then do
            UT.limpiarConsola
            let msj = "El parqueo de salida y el de llegada no pueden ser iguales."
            UT.mostrarMensaje "Error" msj "!!"
            UT.pausarConsola
            solicitarAlquilerBicicleta
        else do
        
        -- Todo en orden, muestra las bicicletas disponibles
        UT.limpiarConsola
        putStrLn "============ [BICICLETAS DISPONIBLES] ============"
        Bicicleta.mostrarBicicletasDeParqueo parqueoSalida path_bicicletas
        putStr "Ingrese el codigo de la bicicleta: "
        codigoBici <- getLine
        bExisteBici <- Bicicleta.bicicletaPerteneceAParqueo codigoBici parqueoSalida path_bicicletas
        if (bExisteBici == True) then do
            id_fac_generado <- obtenerIdFacturaNuevo
            let nuevaFactura = Factura id_fac_generado cedula parqueoSalida parqueoLlegada codigoBici "activo"
            Factura.agregarFactura nuevaFactura
            bic <- Bicicleta.obtenerBicicleta codigoBici "./src/data/bicicletas.json"
            Bicicleta.cambiarParqueo bic "En tránsito" "./src/data/bicicletas.json"
            UT.limpiarConsola
            let msj = "La bicicleta ha sido alquilada. Factura: " ++ show id_fac_generado
            UT.mostrarMensaje "Resultado" msj "!!"
            UT.pausarConsola
            mostrarMenuGeneral
        else do
            UT.limpiarConsola
            let msj = "El código de la bicicleta ingresada no existe."
            UT.mostrarMensaje "Error" msj "!!"
            UT.pausarConsola
            solicitarAlquilerBicicleta
        
        else do
            UT.limpiarConsola
            let msj = "La cédula ingresada no existe."
            UT.mostrarMensaje "Error" msj "!!"
            UT.pausarConsola
            solicitarAlquilerBicicleta
    else do
        existeArchivo <- UT.verificarArchivoExistente direccion
        if (existeArchivo == True) then do
            putStr "Ingrese la cedula del usuario: "
            cedula <- getLine
            bExisteCed <- Usuario.existeCedula cedula "./src/data/usuarios.json"
            if (bExisteCed == True) then do
            -- El usuario existe

            putStr "Ingrese el parqueo de salida: "
            parqueoSalida <- getLine
            putStr "Ingrese el parqueo de llegada: "
            parqueoLlegada <- getLine

            bExisteParqSal <- Parqueo.existeParqueo "./src/data/parqueo.json" parqueoSalida
            bExisteParqLleg <- Parqueo.existeParqueo "./src/data/parqueo.json" parqueoLlegada

            if (bExisteParqSal == False || bExisteParqLleg == False) then do
                UT.limpiarConsola
                let msj = "Debe ingresar parqueos existentes."
                UT.mostrarMensaje "Error" msj "!!"
                UT.pausarConsola
                solicitarAlquilerBicicleta
            else do

            if (parqueoSalida == "En tránsito" || parqueoLlegada == "En tránsito") then do
                UT.limpiarConsola
                let msj = "No se puede alquilar una bicicleta en tránsito."
                UT.mostrarMensaje "Error" msj "!!"
                UT.pausarConsola
                solicitarAlquilerBicicleta
            else do

            if (parqueoSalida == parqueoLlegada) then do
                UT.limpiarConsola
                let msj = "El parqueo de salida y el de llegada no pueden ser iguales."
                UT.mostrarMensaje "Error" msj "!!"
                UT.pausarConsola
                solicitarAlquilerBicicleta
            else do
            
            -- Todo en orden, muestra las bicicletas disponibles
            UT.limpiarConsola
            putStrLn "============ [BICICLETAS DISPONIBLES] ============"
            Bicicleta.mostrarBicicletasDeParqueo parqueoSalida path_bicicletas
            putStr "Ingrese el codigo de la bicicleta: "
            codigoBici <- getLine
            bExisteBici <- Bicicleta.bicicletaPerteneceAParqueo codigoBici parqueoSalida path_bicicletas
            if (bExisteBici == True) then do
                id_fac_generado <- obtenerIdFacturaNuevo
                let nuevaFactura = Factura id_fac_generado cedula codigoBici parqueoSalida parqueoLlegada "En tránsito"
                Factura.agregarFactura nuevaFactura
                bic <- Bicicleta.obtenerBicicleta codigoBici direccion
                Bicicleta.cambiarParqueo bic "En tránsito" direccion
                UT.limpiarConsola
                let msj = "La bicicleta ha sido alquilada."
                UT.mostrarMensaje "Resultado" msj "!!"
                UT.pausarConsola
                mostrarMenuGeneral
            else do
                UT.limpiarConsola
                let msj = "El código de la bicicleta ingresada no existe."
                UT.mostrarMensaje "Error" msj "!!"
                UT.pausarConsola
                solicitarAlquilerBicicleta
            
            else do
                UT.limpiarConsola
                let msj = "La cédula ingresada no existe."
                UT.mostrarMensaje "Error" msj "!!"
                UT.pausarConsola
                solicitarAlquilerBicicleta
        else do
            UT.limpiarConsola
            let msj = "El archivo no existe. Verifique la ruta ingresada."
            UT.mostrarMensaje "Error" msj "!!"
            UT.pausarConsola
            solicitarAlquilerBicicleta

{-
    Función: realizarFacturacion
    Descripción: pemmite realizar la cancelación de una factura activa.
    Objetivo: pide la usuario una factura, muestra el detalle del recibo y lo almacena.
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
realizarFacturacion :: IO ()
realizarFacturacion = do
    putStrLn "============ [REALIZAR FACTURACION] ============"
    putStr "Ingrese el id de la factura: "
    idFactura <- getLine
    
    if (UT.verificarEnteroValido idFactura) then do
        bExisteFactura <- Factura.existeFactura (read idFactura :: Int)
        if (bExisteFactura == True) then do
            UT.limpiarConsola
            factura <- Factura.obtenerFactura (read idFactura :: Int)
            let estadoActualFactura = Factura.estado factura
            if (estadoActualFactura == "facturado") then do
                UT.limpiarConsola
                let msj = "La factura ya se encuentra facturada."
                UT.mostrarMensaje "Error" msj "!!"
                UT.pausarConsola
                mostrarMenuGeneral
            else do
            putStrLn "====================== [DETALLE DE FACTURA] ======================"
            putStrLn ("Número de factura: " ++ show (Factura.idFactura factura))
            putStrLn ("----------------------------------------------")
            Comercio.mostrarComercioSinTarifas
            putStrLn ("----------------------------------------------")
            usuarioFac <- Usuario.obtenerUsuario "./src/data/usuarios.json" (Factura.cedulaCliente factura)
            putStr (Usuario.mostrarUsuario usuarioFac)
            bicicletaFac <- Bicicleta.obtenerBicicleta (Factura.codigoBici factura) "./src/data/bicicletas.json"
            let nuevaBici = Bicicleta (Bicicleta.codigo bicicletaFac) (Bicicleta.tipo bicicletaFac) (Factura.parqueoLlegada factura)
            Bicicleta.actualizarBicicleta nuevaBici "./src/data/bicicletas.json"
            putStrLn "(TR) Tradicional | (AE) Asistencia eléctrica"
            putStr (Bicicleta.mostrarBicicletaEnTransito bicicletaFac)
            parqueoSal <- Parqueo.obtenerParqueo "./src/data/parqueo.json" (Factura.parqueoSalida factura)
            parqueoLleg <- Parqueo.obtenerParqueo "./src/data/parqueo.json" (Factura.parqueoLlegada factura)
            putStrLn $ "-- > Parqueo de salida\n" ++ (Parqueo.mostrarParqueo parqueoSal) ++ "\n"
            putStrLn $ "-- > Parqueo de llegada\n" ++ (Parqueo.mostrarParqueo parqueoLleg)
            let distanciaRecorrida = Parqueo.distanciaEntreDosParqueos parqueoSal parqueoLleg
            putStrLn ("\nDistancia recorrida: " ++ show distanciaRecorrida ++ " km")
            tarifa <- Comercio.obtenerTarifaSegunTipo (Bicicleta.tipo bicicletaFac)
            putStrLn ("Tarifa aplicada: " ++ show tarifa ++ "$ por km")
            let costoTotal = tarifa * distanciaRecorrida
            putStrLn ("Costo total: " ++ show costoTotal ++ "$")
            putStrLn "==============================================================="
            let facturaActualizada = Factura (Factura.idFactura factura) (Factura.cedulaCliente factura) (Factura.parqueoSalida factura) (Factura.parqueoLlegada factura) (Factura.codigoBici factura) "facturado"
            Factura.actualizarFactura facturaActualizada
            let recibo = Recibo (Factura.idFactura factura) (Factura.cedulaCliente factura)  (Factura.parqueoSalida factura) (Factura.parqueoLlegada factura) (Factura.codigoBici factura) (Bicicleta.tipo bicicletaFac) tarifa distanciaRecorrida costoTotal
            Recibo.agregarRecibo recibo
            UT.pausarConsola
            mostrarMenuGeneral
        else do
            UT.limpiarConsola
            let msj = "El id de la factura ingresado no existe."
            UT.mostrarMensaje "Error" msj "!!"
            UT.pausarConsola
            realizarFacturacion
    else do
        UT.limpiarConsola
        let msj = "El id de la factura debe ser un número entero."
        UT.mostrarMensaje "Error" msj "!!"
        UT.pausarConsola
        realizarFacturacion

{-
    Función: mostrarMenuGeneral
    Descripción: muestra el menu de opciones generales.
    Objetivo: enseñarle al usuario todas las opciones generales y navegar a través de sus opciones
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
mostrarMenuGeneral :: IO ()
mostrarMenuGeneral = do
    putStrLn "============ [MENU GENERAL] ============"
    putStrLn "1. Consultar parqueo más cercano (por coordenadas)."
    putStrLn "2. Alquilar bicicleta"
    putStrLn "3. Facturar"
    putStrLn "0. Volver al menú principal"
    putStr "Digite la opcion deseada: \n>"
    opcion <- getLine

    if (UT.verificarEnteroValido opcion) then do
        case (read opcion :: Int) of
            1 -> do
                UT.limpiarConsola
                solicitarParqueoMasCercano
            2 -> do
                UT.limpiarConsola
                solicitarAlquilerBicicleta
            3 -> do
                UT.limpiarConsola
                realizarFacturacion
            0 -> do
                UT.limpiarConsola
                mostrarMenuPrincipal
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

{-
    Función: mostrarMenuPrincipal
    Descripción: muestra el menú de opciones principales.
    Objetivo: enseñarle al usuario todas las opciones principales y navegar a través de sus opciones
    Parámetros: --
    Retorno: --
    Restricciones: --
-}
mostrarMenuPrincipal :: IO ()
mostrarMenuPrincipal = do
    putStrLn "============ [MENU PRINCIPAL] ============"
    putStrLn "1. Opciones operativas"
    putStrLn "2. Opciones generales"
    putStrLn "0. Salir"
    putStr "Digite la opcion deseada: \n>"
    opcion <- getLine

    if (UT.verificarEnteroValido opcion) then do
        case (read opcion :: Int) of
            1 -> do
                UT.limpiarConsola
                mostrarMenuOperativo
            2 -> do
                UT.limpiarConsola
                mostrarMenuGeneral
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

-- Punto de arranque de la aplicación
main :: IO ()
main = do
    UT.limpiarConsola
    mostrarMenuPrincipal
