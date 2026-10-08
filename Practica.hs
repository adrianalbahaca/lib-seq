import Seq
import BTreeSeq

suma :: Seq s => s Int -> Int
suma s = reduceS (+) 0 s

idS :: Seq s => Int -> s Int
idS n = tabulateS (\i -> i) n 

sumaSec :: Seq s => s Int -> (s Int, Int)
sumaSec s = scanS (+) 0 s