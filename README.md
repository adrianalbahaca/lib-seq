# Librería del TAD de Secuencias | EDyAII

Esta es una librería para practicar usando el TAD de Secuencias para la materia de Estructura de Datos y Algoritmos II
- Contiene las funciones de la librería `Seq` (`emptyS`, `tabulateS`, `fromListS`, etc...)
- Se ha implementado usando un árbol binario del tipo `data BTree a = Empty | Node Int (BTree a) a (BTree a)`, donde el entero es el tamaño del árbol, y su recorrido inorder representa la secuencia
- Funciones como `appendS` y `filterS` balancean el árbol a medida que se ejecutan. `lengthS` fue implementado para que tenga costo $O(1)$ y `takeS` y `dropS` tienen costo logarítmico
- Se ha usado `Par` con el operador `|||` para poder paralelizar

## Instalación y uso rápido
Para instalar, en la carpeta de la librería:

```haskell
cabal update
cabal install --lib
```

Luego, para usar en un archivo

```haskell
import Seq
import BTreeSeq

suma :: Seq s => s a -> a
suma s = reduceS (+) 0 s

main :: IO ()
main = print (suma (tabulateS ( + 1) 100) :: BTree Int)
```

Luego, `ghci archivo.hs` o `ghc archivo.hs` encuentra la librería solo. Tome en cuenta que, al fijar `Seq s =>`, es necesario especificar la instancia con anotación de tipo (`:: BTree Int`)

## Paralelismo

Es necesario compilar con hilos para poder usar `|||` adecuadamente

```
ghc -threaded -rtsopts archivo.hs
./archivo +RTS -N
```

## Estado y limitaciones
- `takeS` y `dropS` no aplican balanceo automáticamente
- `scanS` fue implementado de forma ineficiente. Está pendiente optimizarlo
- El árbol se balancea *por tamaño*. Las funciones `fromListS` y `tabulateS` construyen el árbol casi parejo