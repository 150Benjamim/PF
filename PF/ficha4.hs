import Data.Char

--exc2

excA = [2^x | x <- [0..10]]

excB = [(x,y) | x <- [1..5], y <- [1..5], x+y == 6]

excC = [ [1..x] | x <- [1..5]]

excD = [ replicate x 1 | x <- [1..5]]

excE = [ factorial x | x <- [1..6]]
     where factorial 0 = 1
           factorial x = x * factorial (x-1)


--exc3

digitAlpha :: String -> (String,String)
digitAlpha []    = ([],[])
digitAlpha (h:t) = if isAlpha h == True then (h:a,b) else if isDigit h == True then (a,h:b) else (a,b)
                 where (a,b) = digitAlpha t


--exc4

nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h < 0  = (1+a,b,c) 
          | h == 0 = (a,1+b,c)
          | h > 0  = (a,b,1+c)
          where (a,b,c) = nzp t


--exc5 (feito de maneira errada)

divMod' :: Integral a => a -> a -> (a, a)
divMod' x y = (div x y, mod x y)
