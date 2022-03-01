
--exc1

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (h:t) | f h == True = True
             | otherwise = any' f t

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [a] [] = []
zipWith' _ [] [a] = []
zipWith' f (h1:t1) (h2:t2) = (f h1 h2) : zipWith' f t1 t2

takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (h:t) | f h == True = h : takeWhile' f t
                   | otherwise = []

dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (h:t) | f h == True = dropWhile' f t
                   | otherwise = (h:t)

span' :: (a-> Bool) -> [a] -> ([a],[a])
span' f [] = ([],[])
span' f (h:t) | f h == True = ( (h:a) , b )
              | otherwise   = ( [] , (h:t) )
              where (a,b) = span' f t

deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' _ _ [] = []
deleteBy' f x (h:t) | f x h == True = t
                    | otherwise = h : deleteBy' f x t

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' _ [] = []
sortOn' f (h:t) = insereAux f h (sortOn' f t)

insereAux :: Ord b => (a -> b) -> a -> [a] -> [a]
insereAux _ x [] = [x]
insereAux f x (h:t) | (f x) <= (f h) = (x : h : t)
                    | otherwise = h : (insereAux f x t)


--exc2

type Polinomio = [Monomio]
type Monomio = (Float,Int)

selgrau :: Int -> Polinomio -> Polinomio
selgrau n l  = filter (\x -> snd x == n) l

conta :: Int -> Polinomio -> Int
conta n l = length (selgrau n l)

grau :: Polinomio -> Int
grau l = foldl (\acc x -> if acc > snd x then acc else snd x) 0 l

deriv :: Polinomio -> Polinomio
deriv l = filter (/=(0,0)) ( map (\(x,y) -> if y > 0 then (x * fromIntegral y, y-1) else (0,0)) l )

calcula :: Float -> Polinomio -> Float
calcula n l = sum (map (\(x,y) -> x*n^y) l)

simp :: Polinomio -> Polinomio
simp l = filter (\x -> snd x == 0) l

mult :: Monomio -> Polinomio -> Polinomio
mult (a,b) l = map (\(x,y) -> (x*a,b+y)) l

ordena :: Polinomio -> Polinomio
ordena l = sortOn' snd l

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza p@((c,e):t) = normalizaAux (selgrau e p) : normaliza (filter (\p2 -> snd p2 /= e) t)

-- polinomio com monomios de grau igual -> soma para um monomio
normalizaAux :: Polinomio -> Monomio
normalizaAux [] = (0,0)
normalizaAux ((c,e):t) = (c+a,e) where (a,_) = normalizaAux t

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)

produto :: Polinomio -> Polinomio -> Polinomio
produto [] [] = []
produto [] _  = []
produto (h:t) p = normaliza (mult h p ++ produto t p)

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 | ordena (normaliza p1) == ordena (normaliza p2) = True
            | otherwise = False



