


--exc1

data Frac = F Integer Integer


instance Eq Frac where
      (F x1 y1) == (F x2 y2) = x1*y2 == x2*y1

instance Ord Frac where
      (F x1 y1) <= (F x2 y2) = x1*y2 <= x2*y1

instance Show Frac where
      show (F x y) = show x ++ "/" ++ show y

instance Num Frac where
      -- (+), (*), (-) :: a -> a -> a
      -- negate, abs, signum :: a -> a
      -- fromInteger :: Integer -> a
      (F x1 y1) + (F x2 y2) | y1 == y2 = normaliza (F (x1+x2) y1)
                            | otherwise = normaliza (F (x1*y2 + x2*y1) (y1*y2))
      (F x1 y1) * (F x2 y2) = normaliza (F (x1*x2) (y1*y2))
      (F x1 y1) - (F x2 y2) | y1 == y2 = normaliza (F (x1-x2) y1)
                            | otherwise = normaliza (F (x1*y2 - x2*y1) (y1*y2))
      negate (F x y) = normaliza (F (-x) y)
      abs (F x y)    = normaliza (F (abs x) (abs y))
      signum (F x y) | x * y < 0  = -1
                     | x * y == 0 = 0
                     | x * y > 0  = 1
      fromInteger x = (F x 1)

normaliza :: Frac -> Frac
normaliza (F x y) | (x*y > 0) = (F (div (abs x) (mdc (abs x)(abs y))) (div (abs y) (mdc (abs x) (abs y))) )
                  | (x*y < 0) = (F (negate (div x (mdc (abs x)(abs y)))) (div (abs y) (mdc (abs x) (abs y))) )
                  | otherwise = (F 0 0)

mdc :: Integer -> Integer -> Integer
mdc x y = last [n | n <- [1.. (min x y)] , mod x n == 0, mod y n == 0]


maioresDobro :: Frac -> [Frac] -> [Frac]
maioresDobro _ [] = []
maioresDobro f l | (head l) > 2 * f = head l : maioresDobro f (tail l)
                 | otherwise = maioresDobro f (tail l)




--exc2

data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

instance Show a => Show (Exp a) where
      show (Const a)     = show a
      show (Simetrico a) = "-" ++ show a
      show (Mais a b)    = show a ++ "+" ++ show b
      show (Menos a b)   = show a ++ "-" ++ show b
      show (Mult a b)    = show a ++ "*" ++ show b

calcula :: Num a => Exp a -> a
calcula (Const x) = x
calcula (Simetrico x) = - (calcula x)
calcula (Mais x y)    = calcula x + calcula y
calcula (Menos x y)   = calcula x - calcula y
calcula (Mult x y)    = calcula x * calcula y

instance (Eq a, Num a) => Eq (Exp a) where
      x == y = calcula x == calcula y

