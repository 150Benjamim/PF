

--exc1

data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt 
            deriving Show

calcula :: ExpInt -> Int
calcula (Const x) = x
calcula (Simetrico x) = - (calcula x)
calcula (Mais x y)    = calcula x + calcula y
calcula (Menos x y)   = calcula x - calcula y
calcula (Mult x y)    = calcula x * calcula y

infixa :: ExpInt -> String
infixa (Const x)     = show x
infixa (Simetrico x) = "-" ++ infixa x
infixa (Mais x y)    = "(" ++ infixa x ++ "+" ++ infixa y ++ ")"
infixa (Menos x y)   = "(" ++ infixa x ++ "-" ++ infixa y ++ ")"
infixa (Mult x y)    = "(" ++ infixa x ++ "*" ++ infixa y ++ ")"

posfixa :: ExpInt -> String
posfixa (Const x)     = show x
posfixa (Simetrico x) = "-" ++ " " ++ posfixa x
posfixa (Mais x y)    = posfixa x ++ " " ++ posfixa y ++ " " ++ "+"
posfixa (Menos x y)   = posfixa x ++ " " ++ posfixa y ++ " " ++ "-"
posfixa (Mult x y)    = posfixa x ++ " " ++ posfixa y ++ " " ++ "*"




--exc2

rt1 = (R 1 [(R 2 [(R 3 [(R 5 [])])]), (R 3 [])])

data RTree a = R a [RTree a] deriving Show

soma :: Num a => RTree a -> a
soma (R x []) = x
soma (R x t)  = x + sum ( map (soma) t )

altura :: RTree a -> Int
altura (R x []) = 1
altura (R x t) = 1 + maximum ( map (altura) t )

prune :: Int -> RTree a -> RTree a
prune n (R x t) | n == 1 = (R x [])
                | otherwise = (R x (map (prune (n-1)) t))

mirror :: RTree a -> RTree a
mirror (R x t) = ( R x (map mirror (reverse t)) ) 

postorder :: RTree a -> [a]
postorder (R x t)  = concatMap (postorder) t ++ [x]



--exc3

data BTree a = Empty | Node a (BTree a) (BTree a)

data LTree a = Tip a | Fork (LTree a) (LTree a)

ltSum :: Num a => LTree a -> a
ltSum (Tip a) = 1
ltSum (Fork l r) = ltSum l + ltSum r

listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork l r) = listaLT l ++ listaLT r

ltHeight :: LTree a -> Int
ltHeight (Tip a) = 1
ltHeight (Fork l r) = 1 + max (ltHeight l) (ltHeight r)



--exc4

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf b) = (Empty, Tip b)
splitFTree (No a l r) = (Node a l1 r1, Fork l2 r2)
                      where (l1,l2) = splitFTree l
                            (r1,r2) = splitFTree r

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Tip x) = Just (Leaf x)
joinTrees (Node x l1 r1) (Fork l2 r2) = Just (No x aux aux2)
        where Just aux = joinTrees l1 l2
              Just aux2 = joinTrees r1 r2
joinTrees _ _ = Nothing


