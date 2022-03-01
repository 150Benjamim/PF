

--ordena uma lista de ints

ordena :: [Int] -> [Int]
ordena [] = []
ordena (h:t) = ordena [l | l <- t, l < h] ++ [h] ++ ordena [r | r <- t, r >= h]