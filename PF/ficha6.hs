
data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show


--exc1

altura :: BTree a -> Int
altura Empty = 0
altura (Node a l r) = 1 + max (altura l) (altura r)

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node a l r) = 1 + contaNodos l + contaNodos r

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node a Empty Empty) = 1
folhas (Node a l r) = folhas l + folhas r

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune n (Node x l r) | n == 1 = Empty
                     | otherwise = (Node x (prune (n-1) l) (prune (n-1) r))

path :: [Bool] -> BTree a -> [a]
path [] _ = []
path _ Empty = []
path (h:t) (Node x l r) | h == False = x : path t l
                        | h == True  = x : path t r

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node x l r) = (Node x (mirror r) (mirror l))

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node x1 l1 r1) (Node x2 l2 r2) = (Node (f x1 x2) (zipWithBT f l1 l2) (zipWithBT f r1 r2))
zipWithBT _ _ _ = Empty

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (a1,b1,c1) l r) = (Node a1 l1 r1, Node b1 l2 r2, Node c1 l3 r3)
                              where (l1,l2,l3) = unzipBT l
                                    (r1,r2,r3) = unzipBT r



--exc2

minimo :: Ord a => BTree a -> a
minimo (Node x Empty Empty) = x
minimo (Node x l r) = minimo l

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x Empty Empty) = Empty
semMinimo (Node x l r) = semMinimo l

minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node x Empty Empty) = (x,Empty)
minSmin (Node x l r) = (a,Node x b r)
                     where (a,b) = minSmin l

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove n (Node x l r) | n == x = ( Node min l sMin)
                      | n < x  = ( Node x (remove n l) r )
                      | n > x  = ( Node x l (remove n r) )
                      where (min,sMin) = minSmin r




--exc3

type Aluno = (Numero,Nome,Regime,Classificacao)

type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int | Rep | Faltou deriving Show

type Turma = BTree Aluno -- árvore binária de procura (organizada por número)


inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum x (Node (n,_,_,_) l r) | x == n = True
                               | x < n  = inscNum x l
                               | x > n  = inscNum x r

inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome n (Node (_,x,_,_) l r) | n == x = True
                                | otherwise = inscNome n l || inscNome n r

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nom,reg,_) l r) = case reg of TE -> trabEst l ++ [(num,nom)] ++ trabEst r
                                                 otherwise -> trabEst l ++ trabEst r

nota :: Numero -> Turma -> Maybe Classificacao
nota n turma@(Node (num,_,_,clas) l r) | inscNum n turma == False = Nothing
                                       | otherwise = if (n == num) then Just clas else if (n<num) then nota n l else nota n r

percFaltas :: Turma -> Float
percFaltas turma = (faltasTotal turma / alunosTotal turma) * 100
                 where alunosTotal Empty = 0
                       alunosTotal (Node x l r) = 1 + alunosTotal l + alunosTotal r
                       faltasTotal Empty = 0
                       faltasTotal turma@(Node (_,_,_,clas) l r) = case clas of Faltou -> 1 + faltasTotal l + faltasTotal r
                                                                                otherwise -> 0 + faltasTotal l + faltasTotal r

mediaAprov :: Turma -> Float
mediaAprov turma = (totalNotas turma / totalPass turma) * 100
                 where totalNotas Empty = 0
                       totalNotas (Node (_,_,_,clas) l r) = case clas of Aprov x -> (fromIntegral x) + totalNotas l + totalNotas r
                                                                         otherwise -> totalNotas l + totalNotas r
                       totalPass Empty = 0
                       totalPass (Node (_,_,_,clas) l r) = case clas of Aprov x -> 1 + totalPass l + totalPass r
                                                                        otherwise -> totalPass l + totalPass r

aprovAv :: Turma -> Float
aprovAv Empty = 0
aprovAv turma = (a / b)
              where (a,b) = auxAprov turma
                    auxAprov Empty = (0,0)
                    auxAprov (Node (_,_,_,clas) l r) = case clas of Aprov _ -> (1+x,y)
                                                                    Rep     -> (x,1+y)
                                                                    Faltou  -> (x,y)
                     where (x,y) = (c+d,e+f)
                           (c,e) = auxAprov l
                           (d,f) = auxAprov r