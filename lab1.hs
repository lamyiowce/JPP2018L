
triads :: Int -> [(Int, Int, Int)]

-- triads n = [(a,b,c) | a <- [1..n], b <- [1..a], c <- [1..n], a*a + b*b == c*c]

-- triads n = [(a,b,a*a+b*b) | a <- [1..n], b <- [1..n], a*a + b*b <= n && a <= b]

pyth_iter :: Int -> Int -> Int -> [(Int, Int, Int)]
pyth_iter n a b 
    | a > b = []
    | a*a + b*b == n*n = (a, b, n) : (pyth_iter n (a+1) (b-1))
    | a*a + b*b > n*n = pyth_iter n a (b-1)
    | otherwise =  pyth_iter n (a+1) b

find_pyths n = pyth_iter n 1 n

triads n = foldr (++) [] [pyth_iter a 1 a | a <- [1..n]]

euler :: Int -> Int

nwd a b = if b == 0 then a else nwd b (a `mod` b)
euler n = length $ filter (\k -> nwd n k == 1) [1..n-1] 


indexOf :: Char -> String -> Maybe Int

indexOfAux c (s:ss) idx
    | c == s = Just idx
    | ss == [] = Nothing
    | otherwise = indexOfAux c ss (idx+1)
indexOf c ss = indexOfAux c ss 0

positionsAux c [] n = []
positionsAux c (s:ss) n 
    | c == s = n:(positionsAux c ss (n+1))
    | otherwise = positionsAux c ss (n+1)
positions c ss = positionsAux c ss 0

factAux 1 k = k
factAux n k = factAux (n-1) k*n  
fact n = factAux n 1


incAll :: [[Int]] -> [[Int]]
incAll lss = map (map (+1)) lss 

fact_fold n = foldr (*) 1 [1..n]

nub :: [Integer] -> [Integer]
nub (x:[]) = [x]
nub (x:xs) = x : (filter (/=x) $ nub xs)



showInt :: Int -> String

tochar d 
	| d == 0 = '0'
	| otherwise = succ $ tochar (d-1)

showInt 0 = []
showInt n = 
	let 
		aux n 
		   | n == 0 = []
		   | otherwise = (tochar (n `mod` 10)) : (aux $ n `div` 10)
	in 
		reverse $ aux n

showLst f l = '[' : (foldr (\elem -> \acc -> if acc == [] then elem else (elem)++","++acc) [] $ map f l) ++ [']']


width = 30
splitter s = 
    let 
        aux cs k 
           | cs == [] = []
           | k == 0 = "\n" ++ (aux cs width)
           | otherwise = (head cs) : (aux (tail cs) (k-1))
    in
        aux s width

