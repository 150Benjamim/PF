import Data.Char

--exc1

dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = (2*h) : dobros t

numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre x (h:t) | x == h = 1 + numOcorre x t
                  | otherwise = numOcorre x t

positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) | h >= 0 = positivos t
                | otherwise = False

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) | h >= 0 = h : soPos t
            | otherwise = soPos t

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) | h < 0 = h + somaNeg t
              | otherwise = somaNeg t

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt l | (length l) > 3 = tresUlt (tail l)
          | otherwise = l

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):t) = b : segundos t

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros x ((a,b):t) | x == a = True
                         | otherwise = nosPrimeiros x t

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):t) = (a+a2,b+b2,c+c2)
                       where (a2,b2,c2) = sumTriplos t


--exc3

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) | isDigit h == True = h : soDigitos t
                | otherwise = soDigitos t

minisculas :: [Char] -> Int
minisculas [] = 0
minisculas (h:t) | isLower h == True = 1 + minisculas t
                 | otherwise = minisculas t

nums :: String -> [Int]
nums [] = []
nums (h:t) | isDigit h == True = digitToInt h : nums t
           | otherwise = nums t


--exc4

type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta x ((a,b):t) | x == b = 1 + conta x t
                  | otherwise = conta x t

grau :: Polinomio -> Int
grau [] = 0
grau [(a,b)] = b
grau ((a1,b1):(a2,b2):t) | b1 >= b2  = grau ((a1,b1):t)
                         | otherwise = grau ((a2,b2):t)

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau x ((a,b):t) | x == b = (a,b) : selgrau x t
                    | otherwise = selgrau x t

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((a,b):t) | b == 0 = deriv t
                | otherwise = ((a * fromIntegral b),b-1) : deriv t

calcula :: Float -> Polinomio -> Float
calcula x [] = 0
calcula x ((a,b):t) = (a * x^b) + calcula x t

simp :: Polinomio -> Polinomio
simp [] = []
simp ((a,b):t) | b == 0 = simp t
               | otherwise = (a,b) : simp t

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (x,y) ((a,b):t) = (x*a,b+y) : mult (x,y) t

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((a1,b1):t) = normalizaAux (a1,b1) t : normaliza (normalizaAuxD (a1,b1) t)

normalizaAux :: Monomio -> Polinomio -> Monomio
normalizaAux (a,b) [] = (a,b)
normalizaAux (a1,b1) ((a2,b2):t) | b1 == b2  = normalizaAux (a1+a2,b1) t
                                 | otherwise = normalizaAux (a1,b1) t

normalizaAuxD :: Monomio -> Polinomio -> Polinomio
normalizaAuxD (a,b) [] = []
normalizaAuxD (a1,b1) ((a2,b2):t) | b1 == b2 = normalizaAuxD (a1,b1) t
                                  | otherwise = (a2,b2) : normalizaAuxD (a1,b1) t

soma :: Polinomio -> Polinomio -> Polinomio
soma [] l = l
soma l [] = l
soma p1 p2 = normaliza (p1 ++ p2)

produto :: Polinomio -> Polinomio -> Polinomio
produto [] l = l
produto l [] = l
produto ((a,b):t) p2 = soma (mult (a,b) p2) (produto t p2)

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((a,b):t) = insereAux (a,b) (ordena t)

insereAux :: Monomio -> Polinomio -> Polinomio
insereAux p [] = [p]
insereAux (a1,b1) ((a2,b2):t) | b1 <= b2 = (a1,b1) : ((a2,b2):t)
                              | b1 > b2  = (a2,b2) : insereAux (a1,b1) t

equiv :: Polinomio -> Polinomio -> Bool
equiv [] p = False
equiv p [] = True
equiv p1 p2 | ordena (normaliza p1) == ordena (normaliza p2) = True
           | otherwise = False

