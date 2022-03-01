
--exc1

perimetro :: Float -> Float
perimetro x = pi * 2 * x

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1,y1) (x2,y2) = sqrt ( (x2-x1)^2 + (y2-y1)^2 )

primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)

multiplo :: Int -> Int -> Bool
multiplo x n | mod x n == 0 = True
             | otherwise = False

truncaImpar :: [a] -> [a]
truncaImpar [] = []
truncaImpar l | mod (length l) 2 == 0 = l
              | otherwise = tail l

max2 :: Int -> Int -> Int
max2 x y | x >= y = x
         | x < y  = y 

max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 (max2 x y) z



--exc2

nRaizes :: Float -> Float -> Float -> Int
nRaizes a b c | sqrtFormula a b c < 0  = 0
              | sqrtFormula a b c == 0 = 1
              | otherwise              = 2


sqrtFormula :: Float -> Float -> Float -> Float
sqrtFormula a b c = b^2 - 4*a*c

raizes :: Float -> Float -> Float -> Float
raizes a b c = (b - sqrtFormula a b c) / 2*a

