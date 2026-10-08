module BTreeSeq where
    
    import Seq
    import Par

    data BTree a = Empty | Node Int (BTree a) a (BTree a)

    -- Funciones auxiliares

    node :: BTree a -> a -> BTree a -> BTree a
    node l x r = Node (lengthT l + 1 + lengthT r) l x r
    
    sacarPrimero :: BTree a -> BTree a
    sacarPrimero Empty = Empty
    sacarPrimero (Node _ Empty a r) = r
    sacarPrimero (Node _ l x r) = nodeB (sacarPrimero l) x r 
    
    -- TODO: Crear una función que balancee un árbol con tamaño sin romper
    -- invariante de recorrido inorder pedido en Seq

    -- Para evaluar cuándo un árbol esta desbalanceado, se hace una condición de tipo peso
    peso :: Int
    peso = 2

    -- Es el árbol a "más grande" que el árbol b?
    muyGrande :: BTree a -> BTree a -> Bool
    muyGrande a b = (lengthT a) + 1 > peso * (lengthT b + 1)

    desbalanceado :: BTree a -> BTree a -> Bool
    desbalanceado t1 t2 = muyGrande t1 t2 || muyGrande t2 t1

    reconstruir :: BTree a -> BTree a
    reconstruir t = tabulateT (nthT t) (lengthT t)

    nodeB :: BTree a -> a -> BTree a -> BTree a
    nodeB l x r
        | desbalanceado l r = reconstruir (node l x r)
        | otherwise = node l x r

    link :: BTree a -> a -> BTree a -> BTree a
    link l x r
        | muyGrande l r = case l of
            Node _ ll y lr -> nodeB ll y (link lr x r)
        | muyGrande r l = case r of
            Node _ rl y rr -> nodeB (link l x rl) y rr
        | otherwise = node l x r 

    -- // -- // -- // -- // --

    emptyT :: BTree a
    emptyT = Empty

    singletonT :: a -> BTree a
    singletonT a = Node 1 Empty a Empty

    lengthT :: BTree a -> Int
    lengthT Empty = 0
    lengthT (Node s _ _ _) = s

    nthT :: BTree a -> Int -> a
    nthT Empty _ = error "Seq: índice fuera de rango"
    nthT (Node s l x r) i
        | i < k = nthT l i
        | i == k = x
        | otherwise = nthT r (i - k - 1)
        where k = lengthT l
    
    tabulateT :: (Int -> a) -> Int -> BTree a
    tabulateT _ n | n < 0 = error "Seq: Número natural necesario"
    tabulateT _ 0 = emptyT
    tabulateT f 1 = singletonT (f 0)
    tabulateT f n =
        let
            medio = n `div` 2
        in
            node (tabulateT f medio) (f medio) (tabulateT (\i -> (f (i + medio + 1))) (n - medio - 1))

    mapT :: (a -> b) -> BTree a -> BTree b
    mapT _ Empty = emptyT
    mapT f (Node s l x r) =
        let
            (ml, mr) = (mapT f l) ||| (mapT f r)
        in
            Node s ml (f x) mr
    
    filterT :: (a -> Bool) -> BTree a -> BTree a
    filterT _ Empty = Empty
    filterT pred (Node s l x r)
        | (pred x) = node fl x fr
        | (lengthT fr == 0) = fl
        | otherwise =
            let a = nthT fr 0
            in nodeB fl a (sacarPrimero fr)
        where (fl, fr) = (filterT pred l) ||| (filterT pred r)
    
    appendT :: BTree a -> BTree a -> BTree a
    appendT Empty a = a
    appendT a Empty = a
    appendT tl tr = 
        let
            a = nthT tr 0
        in link tl a (sacarPrimero tr)
    
    takeT :: BTree a -> Int -> BTree a
    takeT Empty _ = Empty 
    takeT (Node s l x r) n
        | ll == n = l
        | ll > n = takeT l n
        | otherwise = node l x (takeT r (n - ll - 1))
        where ll = lengthT l
    
    dropT :: BTree a -> Int -> BTree a
    dropT Empty _ = Empty
    dropT t@(Node s l x r) n
        | n == 0 = t
        | n >= s = Empty
        | n > ll = dropT r (n - ll - 1)
        | otherwise = node (dropT l n) x r
        where ll = lengthT l
    
    toListT :: BTree a -> [a]
    toListT Empty = []
    toListT (Node _ l x r) = (toListT l) ++ [x] ++ (toListT r)

    joinT :: BTree (BTree a) -> BTree a
    joinT Empty = Empty
    joinT (Node _ l t r) =
        let (ll, rr) = joinT l ||| joinT r
        in appendT ll (appendT t rr)
    
    reduceT :: (a -> a -> a) -> a -> BTree a -> a
    reduceT f n Empty = n
    reduceT f n (Node _ l x r) =
        let (rl, rr) = reduceT f n l ||| reduceT f n r
        in f rl (f x rr)
    
    scanT :: (a -> a -> a) -> a -> BTree a -> (BTree a, a)
    scanT f b t = (tabulateT (\i -> reduceT f b (takeT t i)) (lengthT t), reduceT f b t)
    -- ADVERTENCIA: Esta forma de aplicarlo es SUPER ineficiente

    fromListT :: [a] -> BTree a
    fromListT [] = Empty
    fromListT (x:xs) = appendT (singletonT x) (fromListT xs)

    instance Show a => Show (BTree a) where
        show = show . toListT
    
    instance Seq BTree where
        emptyS = emptyT
        singletonS = singletonT
        lengthS = lengthT
        nthS = nthT
        tabulateS = tabulateT
        mapS = mapT
        filterS = filterT
        appendS = appendT
        takeS = takeT
        dropS = dropT
        showtS Empty = EMPTY
        showtS (Node _ Empty x Empty) = ELT x
        showtS (Node _ Empty x r) = NODE (singletonT x) r
        showtS (Node _ l x r) = NODE l (node Empty x r)

        showlS Empty = NIL
        showlS t@(Node _ l x r) =
            let a = nthT t 0
            in CONS a (sacarPrimero t)

        joinS = joinT
        reduceS = reduceT
        scanS = scanT
        fromListS = fromListT


        

