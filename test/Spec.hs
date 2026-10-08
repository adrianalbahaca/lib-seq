module Main (main) where

import Control.Monad (unless)
import System.Exit (exitFailure)

import Seq
import BTreeSeq

-- Profundidad de un árbol (solo para tests: es O(n)).
profundidad :: BTree a -> Int
profundidad Empty          = 0
profundidad (Node _ l _ r) = 1 + max (profundidad l) (profundidad r)

-- Cota generosa: el doble del mínimo posible.
cotaProfundidad :: Int -> Int
cotaProfundidad n = 2 * ceiling (logBase 2 (fromIntegral (n + 1) :: Double))

secuencia :: Int -> BTree Int
secuencia n = tabulateS id n

pruebas :: [(String, Bool)]
pruebas =
    [ ("tabulateS", toListT (tabulateS (* 2) 5) == [0, 2, 4, 6, 8])
    , ("lengthS / nthS", lengthS (secuencia 7) == 7 && nthS (secuencia 7) 3 == 3)
    , ("mapS", toListT (mapS (+ 1) (secuencia 4)) == [1, 2, 3, 4])
    , ("filterS", toListT (filterS even (secuencia 10)) == [0, 2, 4, 6, 8])
    , ("filterS (raíz no cumple, sin derecha)", toListT (filterS even (secuencia 2)) == [0])
    , ("appendS", toListT (appendS (secuencia 2) (secuencia 3)) == [0, 1, 0, 1, 2])
    , ("appendS con vacíos", toListT (appendS emptyS (secuencia 3)) == [0, 1, 2]
                              && toListT (appendS (secuencia 3) emptyS) == [0, 1, 2])
    , ("takeS / dropS", toListT (takeS (secuencia 5) 3) == [0, 1, 2]
                        && toListT (dropS (secuencia 5) 3) == [3, 4])
    , ("reduceS", reduceS (+) 0 (secuencia 101) == 5050)
    , ("scanS", let (s, t) = scanS (+) 0 (mapS (+ 1) (secuencia 6))
                in toListT s == [0, 1, 3, 6, 10, 15] && t == 21)
    , ("joinS", toListT (joinS (fromListS [secuencia 2, secuencia 0, secuencia 3]))
                == [0, 1, 0, 1, 2])
    , ("fromListS", toListT (fromListS [5, 4, 3, 2, 1 :: Int]) == [5, 4, 3, 2, 1])
    , ("showtS", case showtS (secuencia 4) of
                   NODE l r -> toListT l ++ toListT r == [0, 1, 2, 3]
                               && lengthS l > 0 && lengthS r > 0
                   _        -> False)
    , ("showlS", case showlS (secuencia 3) of
                   CONS x r -> x == 0 && toListT r == [1, 2]
                   NIL      -> False)
    , ("1000 appendS seguidos: orden", toListT crecida == [1 .. 1000])
    , ("1000 appendS seguidos: balance", profundidad crecida <= cotaProfundidad 1000)
    , ("fromListS: balance", profundidad (fromListS [1 .. 1000 :: Int])
                             <= cotaProfundidad 1000)
    ]
  where
    crecida :: BTree Int
    crecida = foldl appendS emptyS (map singletonS [1 .. 1000])

main :: IO ()
main = do
    let fallas = [nombre | (nombre, ok) <- pruebas, not ok]
    mapM_ (\n -> putStrLn ("FALLA: " ++ n)) fallas
    putStrLn (show (length pruebas - length fallas) ++ "/" ++ show (length pruebas) ++ " pruebas OK")
    unless (null fallas) exitFailure