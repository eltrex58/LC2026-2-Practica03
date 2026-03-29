# Lógica Computacional 2026-2

## Práctica 3

Para trabajar sobre esta base, tienen que hacer un fork de este repositorio y trabajar sobre él.

Únicamente modifiquen el archivo Practica03.hs que se encuentra en el directorio src. Si quieren modificar las pruebas o agregar más, pueden preguntarme con confianza y les explico como modificarlas.

Deben tener instalado el compilador de Haskell para poder probar su práctica. Para ello deben colocarse en el directorio src y ejecutar el comando `ghci Practica03.hs`.

Si quieren probar su práctica haciendo uso de las pruebas unitarias que les estoy pasando, tienen que ejecutar los siguientes comandos desde el directorio donde se encuentra este ReadMe:
```
cabal build
cabal test
```

El primero es para compilar y el segundo es para ejecutar las pruebas unitarias.

Si no les llegan a funcionar, es posible que el problema es que tengan una versión diferente de cabal y de ghc. Si ese es el caso, pueden ejecutar el comando `ghc-pkg list base` para reemplazar la versión base que viene en el archivo .cabal en las líneas 70 y 102.

En este caso particular es posible que también necesiten ejecutar `ghc-pkg list deepseq` para igualmente reemplazar la versión de ese paquete que viene en el archivo .cabal en la línea 104.

## Integrantes

Vazquez Merino Lenin Quetzal
    - No. de Cuenta: 425106914 
+ Islas Garcia Fernando
    - No. de Cuenta: 32229531 
## Comentarios

En el caso de las pruebas unitarias no el Tt que no pasa se debe a que se elmiminan las repeticiones al pasar de la proposicion a la lista d clausuras, sin embargo, preferí dejarlo así ya que para el algoritmo de saturacion no me tengo que preocupar de las repeticiones, incluso creo yo que es más eficiente de esta manera,
Para el primer error la verdad quien sabe que pasa, pero según las definiciones para pasar a FNC esta bien definida tanto en la funcion principal tanto como en la axiliar. 

Cómo dato esxtra, estaba viendo que támbien es posible realizar el algoritmo de saturacion usando el teorema de refutación, de manera que para saber si es satisfacible o no ya no es necesario negar el resultado final, sino que simplemente se niega la lisa de clausulas(esto suna raro, pero ps solo se niega cada elemento). Para concluir, cabe aclarar que esta funcion devuelve si la funcion si es satisfacible(por eso la negacion, pues mi algoritmo axiliar devuelve si la funcion es insatisfacible o no), puse el axilar con rspecto a insatisfacible pues pense que eso se queria saber, pero ps era lo contrario.

UNA COSA MÁS, IMPLEMENTAMOS TODAS LAS FUNCIONES AXILIARES NECESARIAS, CUALQUIER DUDA HACERCA DE FUNCIONES DE HASKELL ESPERO QUE NOS LAS PUEDA COMENTAR, PUES ES UNA PENA QUE POR USAR FUNCIONES TAN SENCILLAS SE NOS DESCUENTE PUNTOS(no digo ue eso este mal, de hecho creo que esta bien porque asi aprendemos más), GRACIAS Y BUENA SEMANA SANTA.