module Genetico 
    (Cromosoma(Cr),
     Datos(Dt),
     algoritmoG,
     poblacionInicial,
     cruzaPadres,
     mutaciones,
     muta,
     torneos,
     ganadorTorneo,
     calcFitness,
     mostrarResultado
    ) where --Hemos añadido todas las funciones menos las auxiliares

import System.Random
import PilaL

--Para poder manejar mejor el cromosoma y los datos hemos decidido que sean Data de tipo Registro
data Cromosoma = Cr {
    genes :: [Int],
    fitness :: Int
} deriving (Show)

data Datos = Dt {
    tamPila1 :: Int,
    tamPila2 :: Int,
    cartitas :: [Int]
} deriving (Show)

algoritmoG :: [Int] -> [Int] -> IO ()
algoritmoG tams cartas = do
    --Creamos los datos a partir de los leídos del fichero
    let datos = Dt {tamPila1 = tams !! 0 , tamPila2 = tams !! 1 , cartitas = cartas}
    --Los siguientes parámetros se pueden modificar a necesidad del tamaño del problema
    let numGeneraciones = 100
    let cromosomasGeneracion = 100
    let numPadres = 50 
    let partTorneo = 10
    let long = length cartas
    gen <- newStdGen --Generamos un Gen aleatorio para las funciones "puras" de haskell (Nos permitirá generar números aleatorios)
    putStrLn "Modo Debug : [S] Si    Default: [N] No" --Para ver la población inicial y final
    res <- getLine
    if res == "S" then do
        let poblacion = poblacionInicial long cromosomasGeneracion gen datos --Generamos la población inicial con los parámetros
        let final = bucleGenetico gen poblacion numGeneraciones numPadres cromosomasGeneracion partTorneo datos --Empezamos el algoritmo
        let mejor = ganadorTorneo final --Finalmente de los cromosomas de la última generación nos quedamos con el mejor
        putStrLn "Población inical:"
        putStrLn (show poblacion)
        putStrLn "Población final:"
        putStrLn (show final)
        mostrarResultado mejor datos --Tras mostrar las poblaciones, mostramos con pilas y formato el mejor resultado
    else do
        let poblacion = poblacionInicial long cromosomasGeneracion gen datos
        let final = bucleGenetico gen poblacion numGeneraciones numPadres cromosomasGeneracion partTorneo datos
        let mejor = ganadorTorneo final
        mostrarResultado mejor datos
    


bucleGenetico :: RandomGen g => g -> [Cromosoma] -> Int -> Int -> Int -> Int -> Datos ->[Cromosoma]
bucleGenetico gen cs numG numP nCr k datos
    | numG == 1 = nuevaPoblacion --Si solo queda una generación más (generación final)
    | otherwise = bucleGenetico nuevoGen2 nuevaPoblacion (numG - 1) numP nCr k datos -- Si no, generamos una nueva y repetimos el proceso
        where
            (nuevoGen1,padres) = torneos gen numP k cs [] --Los padres salen de un torneo
            (nuevoGen2,resto) = torneos nuevoGen1 (nCr - numP) k cs [] --Igualmente el resto de la población que pasará sin cruzar
            hijos = cruzaPadres nuevoGen2 padres datos --Los padres son sustituidos por sus hijos
            nuevaPoblacion = mutaciones nuevoGen2 (hijos ++ resto) datos --La nueva población es el conjunto de las hijos y resto mutados


calcFitness :: [Int] -> Datos -> Int
calcFitness gs datos = (abs ((tamPila1 datos) - dif1)) + (abs ((tamPila2 datos) - dif2)) --Suma de las diferencias en valor absoluto de las pilas y su "marca"
    where --Suma de solo los elementos correspondientes a cada pila
        dif1 = foldl (\ ac (i,g) -> ac + ((cartitas datos) !! i) ) 0 (filter (\ (i,g) -> g == 0 ) (zip [0..] gs ))
        dif2 = foldl (\ ac (i,g) -> ac + ((cartitas datos) !! i) ) 0 (filter (\ (i,g) -> g == 1 ) (zip [0..] gs ))


poblacionInicial :: RandomGen g => Int -> Int -> g -> Datos ->[Cromosoma]
poblacionInicial long n gen datos
    | n < 1 = []
    | otherwise = [ Cr {genes=genesR , fitness = fitnessG} ] ++ poblacionInicial long (n-1) nuevoGen datos
        where
            genesR = take long $ randomRs (0, 1) gen --Generamos secuencias aleatorias de tamaño long de 0s y 1s
            fitnessG = calcFitness genesR datos
            nuevoGen = snd $ next gen --Generamos un nuevo gen, ya que randomRs no lo hace

cruzaPadres :: RandomGen g => g -> [Cromosoma] -> Datos -> [Cromosoma]
cruzaPadres gen [] datos = [] --Caso base
cruzaPadres gen [p] datos = [p] --En caso de tener un número de padres impar
cruzaPadres gen (p1:p2:ps) datos = [Cr {genes = gh1 , fitness = f1 },Cr {genes = gh2 , fitness = f2 }] ++ (cruzaPadres nuevoGen ps datos) 
    where
        gh1 = (take nR (genes p1)) ++ (drop nR (genes p2)) --Los hijos con la unión de los padres en un punto aleatorio
        gh2 = (take nR (genes p2)) ++ (drop nR (genes p1))
        long = length (genes p1)
        (nR,nuevoGen) = randomR (1,long-1) gen
        f1 = calcFitness gh1 datos --Calculamos los fitness de los nuevos cromosomas
        f2 = calcFitness gh2 datos


 --Esta función devuelve un gen para continuar con la alearoriedad y no repetir elementos en los torneos
 --Su funcionalidad es elegir de forma aleatoria n elementos de una población de cromosomas
selecAleatorios :: RandomGen g => g -> Int -> [Cromosoma] -> [Cromosoma] ->(g,[Cromosoma])
selecAleatorios gen n cs acc
    | n == 1 = (nuevoGen,[cs !! nR] ++ acc)
    | otherwise = selecAleatorios nuevoGen (n-1) cs (acc ++ [cs !! nR] )
        where
            long = length cs
            (nR,nuevoGen) = randomR (0,long-1) gen

--Elige k elementos aleatorios y se queda con el mejor n veces (obteniendo una población de n elementos)
torneos :: RandomGen g => g -> Int -> Int -> [Cromosoma] -> [Cromosoma] -> (g,[Cromosoma]) --Donde n es el número de torneos y k el número de participantes en el torneo
torneos gen n k cs acc
    | n == 1 = (nuevoGen,acc ++ [ganadorTorneo aleatorios])
    | otherwise = torneos nuevoGen (n-1) k cs (acc ++ [ganadorTorneo aleatorios])
        where
            (nuevoGen,aleatorios) = selecAleatorios gen k cs []

ganadorTorneo :: [Cromosoma] -> Cromosoma
ganadorTorneo cs = ganadorTorneoAux (tail cs) (head cs)

--Como nos encontramos con un problema de minimización, buscamos el que tenga menor diferencia en las pilas
ganadorTorneoAux :: [Cromosoma] -> Cromosoma -> Cromosoma
ganadorTorneoAux [] ac = ac
ganadorTorneoAux (cr:cs) ac
    | fitness cr < fitness ac = ganadorTorneoAux cs cr --Tenemos que buscar el que menos fitness tenga, ya que nos encontramos en un problema de minimización
    | otherwise = ganadorTorneoAux cs ac

--Elegimos una posición aleatoria y cambiamos su valor
mutaciones :: RandomGen g => g -> [Cromosoma] -> Datos ->[Cromosoma]
mutaciones gen [] datos = []
mutaciones gen (c:cs) datos = [(muta pos c datos)] ++ (mutaciones nuevoGen cs datos)
    where
        long = length (genes c)
        (pos,nuevoGen) = randomR (0,long-1) gen

--Esta función recibe una posición aleatoria y un cromosoma. De forma recursiva debe cambiar el bit de la posición indicada
muta :: Int -> Cromosoma -> Datos ->Cromosoma   -- Actualización de los genes y fitness del nuevo cromosoma obtenido de mutaAux
muta pos cr datos = Cr {genes=nuevosGenes, fitness = nuevoFitness}
    where
        nuevosGenes = mutaAux pos (genes cr)
        nuevoFitness = calcFitness nuevosGenes datos

mutaAux :: Int -> [Int] -> [Int]
mutaAux pos [] =  []
mutaAux pos gs 
    | (gs !! pos) == 1 = (take pos gs) ++ [0] ++ (drop (pos+1) gs) -- Cojo pos primeros, añado el bit nuevo, no cojo hasta pos+1
    | otherwise = (take pos gs) ++ [1] ++ (drop (pos+1) gs)

--Formatea el cromosoma dado como parámetro a un resultado legible
mostrarResultado :: Cromosoma -> Datos -> IO ()
mostrarResultado cr datos = do
    let (p1,p2) = calculaResultado (genes cr) (cartitas datos) vacia vacia
    putStrLn "------------------------"
    putStrLn "Resultado del algoritmo"
    putStr "Genes: "
    putStrLn (show (genes cr))
    putStr "Fitness: "
    putStrLn (show (fitness cr) )
    putStrLn "Pilas: "
    putStr "Pila 1 (Meta="
    putStr (show (tamPila1 datos))
    putStr ") :"
    putStrLn (show p1)
    putStr "Pila 2 (Meta="
    putStr (show (tamPila2 datos))
    putStr ") :"
    putStrLn (show p2)
    putStrLn "------------------------"

--Datos unos genes, los transforma a pilas, teniendo así una representación más natural
calculaResultado :: [Int] -> [Int] -> Pila Int -> Pila Int ->(Pila Int,Pila Int)
calculaResultado [] _ p1 p2 = (p1,p2)
calculaResultado (g:gs) (c:cartas) p1 p2
    | g == 0 = calculaResultado gs cartas (apila c p1) (p2)
    | otherwise = calculaResultado gs cartas (p1) (apila c p2) 