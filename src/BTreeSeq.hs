module BTreeSeq where
    
    import Seq
    import Par

    data BTree a = Empty | Node Int (BTree a) a (BTree a)

    -- Funciones auxiliares

    node :: BTree a -> a -> BTree a -> BTree a
    node l x r = Node (lengthT l + 1 + lengthT r) l x r
    
    sacarPrimero :: BTree a -> BTree a
    sacarPrimero Empty = Empty
    sacarPrimero (Node _ Empty _ r) = r
    sacarPrimero (Node _ l x r) = nodeB (sacarPrimero l) x r

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
            Empty -> error ""
        | muyGrande r l = case r of
            Node _ rl y rr -> nodeB (link l x rl) y rr
            Empty -> error ""
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
    nthT (Node _ l x r) i
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
    filterT p (Node _ l x r)
        | (p x) = node fl x fr
        | (lengthT fr == 0) = fl
        | otherwise =
            let a = nthT fr 0
            in nodeB fl a (sacarPrimero fr)
        where (fl, fr) = (filterT p l) ||| (filterT p r)
    
    appendT :: BTree a -> BTree a -> BTree a
    appendT Empty a = a
    appendT a Empty = a
    appendT tl tr = 
        let
            a = nthT tr 0
        in link tl a (sacarPrimero tr)
    
    takeT :: BTree a -> Int -> BTree a
    takeT Empty _ = Empty 
    takeT (Node _ l x r) n
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
    reduceT _ n Empty = n
    reduceT f n (Node _ l x r) =
        let (rl, rr) = reduceT f n l ||| reduceT f n r
        in f rl (f x rr)

    -- Scan óptimo usando árboles hace 2 pasadas. Una tomando su total y otra usandolo para calcular
    -- Árbol auxiliar: cada nodo guarda el total de su subárbol
    data TT a = TE | TN a (TT a) (TT a)

    -- Total de un nodo, a partir de x y los totales de sus hijos
    nodoTot :: (a -> a -> a) -> a -> TT a -> TT a -> a
    nodoTot _ x TE TE = x
    nodoTot f x (TN a _ _) TE = f a x
    nodoTot f x TE (TN b _ _) = f x b
    nodoTot f x (TN a _ _) (TN b _ _) = f a (f x b)

    -- PASADA 1 (abajo -> arriba): total de cada subárbol
    totales :: (a -> a -> a) -> BTree a -> TT a
    totales _ Empty = TE
    totales f (Node _ l x r) =
        let (tl, tr) = totales f l ||| totales f r
        in TN (nodoTot f x tl tr) tl tr

    -- PASADA 2 (arriba -> abajo): reparte las semillas
    -- e = "lo que hay antes de este subárbol"
    scanAux :: (a -> a -> a) -> a -> BTree a -> TT a -> BTree a
    scanAux _ _ Empty _ = emptyT
    scanAux f e (Node _ l x r) (TN _ ttl ttr) =
        let acum = case ttl of
                TE       -> e
                TN t _ _ -> f e t
            (l', r') = scanAux f e l ttl  |||  scanAux f (f acum x) r ttr
            medio    = acum
        in node l' medio r'

    -- Función principal
    scanTNuevo :: (a -> a -> a) -> a -> BTree a -> (BTree a, a)
    scanTNuevo f e t =
        let tt = totales f t
            s  = scanAux f e t tt
            total = case tt of
                    TE        -> e
                    TN t0 _ _ -> f e t0
        in (s, total)

    -- El Scan se especifica para funciones asociativas como sigue
    -- scanT f b t = (tabulateT (\i -> reduceT f b (takeT t i)) (lengthT t), reduceT f b t)
    -- ADVERTENCIA: Esta forma de aplicarlo es SUPER ineficiente y sólo funciona para funciones asociativas

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
        showlS t =
            let a = nthT t 0
            in CONS a (sacarPrimero t)

        joinS = joinT
        reduceS = reduceT
        scanS = scanTNuevo
        fromListS = fromListT


        

