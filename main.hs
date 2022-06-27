import Control.Exception --Para las Excepciones
import Genetico

--Lectura del fichero usando excepciones if y case of, para evitar errores al crear en un futuro los datos
main :: IO ()
main = do
    putStrLn "Por favor, proporciona el nombre del fichero con los datos:"
    fichero <- getLine
    texto <- catch (readFile fichero) (\err -> print (err::SomeException) >> return "")
    if texto == "" then do
        putStrLn "\ESC[31mEl fichero no existe ó se encuentra vacío\ESC[0m" --Formato de color rojo para mostrar el error
        main
    else do
        let lineas = lines texto
        if (length lineas) < 2 then do
            putStrLn "\ESC[31mFormato incorrecto en el fichero\ESC[0m"
            main
        else do
            let tamPilasS = words (head lineas) --Sacamos los tamaños de las pilas
            case tamPilasS of --Miramos el tamaño de las pilas
                (t1:t2:_) -> do --Como mínimo, en la primera línea tenemos que tener dos tamaños, el resto no importa
                    let tamPilas = [ read ss :: Int| ss <- tamPilasS ] --Parse a Int
                    let cartasS = words (head (drop 1 lineas))
                    case cartasS of
                        (c1:c2:_) -> do
                            let cartas = [ read ss :: Int| ss <- cartasS ] --Parse a Int
                            algoritmoG tamPilas cartas
                            --Llamamos al algoritmo genético
                        _ -> do
                            putStrLn "\ESC[31mSon necesarias al menos 2 cartas\ESC[0m"
                            main
                _ -> do --En caso contrario mal
                    putStrLn "\ESC[31mSon necesarios al menos dos tamaños\ESC[0m"
                    main