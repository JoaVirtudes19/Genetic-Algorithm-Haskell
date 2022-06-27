Compilar:
    ghc .\main.hs -o AG
Lanzar (No son necesarios parámetros de entrada, ya que el programa te los pide manualmente al iniciarlo):
    .\AG.exe
    ##Seguidamente pide nombre del fichero con los datos
        Ejemplos/prueba.txt
        Ejemplos/prueba2.txt
        Ejemplos/pruebaExacta.txt
        Ejemplos/pruebaMAL.txt
        Ejemplos/pruebaMAL2.txt
        nombreDeFicheroNoExistente (Probar con un fichero que no existe)
    ##Además pregunta si desea ver el modo Debug (muestra población inicial y final además del resultado)
        S : (Modo debug)
        N ó pulsar intro : (Solo muestra el resultado) 