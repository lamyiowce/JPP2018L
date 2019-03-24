-- i) write your own functions head, tale, concat, take and drop
--
-- > my_head [3, 7]
-- 3
--
-- > my_head []
-- Exception: empty list
--
-- > my_tale [3, 19, 2]
-- 2
--
-- > my_concat [32, 11] [0, 5, 0]
-- [32, 11, 0, 5, 0]
--
-- > my_take 2 [1, 2, 4, 8]
-- [1, 2]
--
-- > my_drop 2 [1, 2, 4, 8, 16] 
-- [4, 8, 16]

my_tail [] = error "empty"
my_tail (x:[]) = x
my_tail (x:xs) = my_tail xs

my_head [] = error "empty"
my_head (x:_) = x

my_concat x y = flip (foldr (:)) x y

best_concat xs = foldr (++) [] xs

my_take :: Integer -> [a] -> [a]

my_take 0 _ = []
my_take n [] = error "emptyyy"
my_take n (x:xs) = x:(my_take (n-1) xs)

-- ii) write a function inits, which takes as an argument a list and returns the list of its prelists
--
-- > inits [0, 1, 2]
-- [[],[0],[0,1],[0,1,2]]

inits [] = [[]]
inits (x:xs) = [] : map (\l -> x:l) (inits xs)

-- iii) write a function sublists, which takes as an argument a list and returns the list of its sublists
--
-- > > sublists [0, 1]
-- [[],[1],[0],[0,1]]
sublists [] = [[]]
sublists (x:xs) =  (my_concat (map (\l -> x:l) ss) ss) where ss = sublists xs




-- iv) write a function partitions, which takes as an argument a list l and returns the list of its partitions, i.e. the list of couples (l1, l2) such that l1 ++ l2 = l
--
-- > partitions [0, 1, 2]
--[([],[0,1,2]),([0],[1,2]),([0,1],[2]),([0,1,2],[])]

-- partitions l = [(take i l, drop i l) | i <- [0.. length l]]
ends [] = [[]]
ends (x:xs) = map (\l -> l ++ [x]) (ends xs) ++ [[]]
partitions (x:xs) = ([], x:xs) : map (\(a, b) -> (x:a, b)) (partitions xs)
partitions [] = [([],[])]
-- v) write a function second_max, which takes as an argument a list of integers, and returns its second maximal element without sorting it! Can you do it in a way such that the list is read only once?
--
-- > second_max [7, 12, 8, 0, 6]
-- 8
--
-- > second_max [6, 12, 8, 12]
-- 12

second_max :: [Integer] -> Integer --, Integer)

second_max [] = error "empty"
second_max (x:[]) = error "not enough elemts"
second_max (a:b:xs) = snd (foldr (\x -> \(mx, mx_sec) -> if x >= mx then (x, mx) else if x >= mx_sec then (mx, x) else (mx, mx_sec)) (a, b) xs)
-- vi) write a function permutations, which takes as an argument a list, and returns the list of its permutations
--
-- > permutations [0, 1, 2]
-- [[0,1,2],[0,2,1],[1,0,2],[2,0,1],[1,2,0],[2,1,0]]

inserts :: a -> [a] -> [[a]]
inserts x [] = [[x]]
inserts x (y:ys) = [(x:y:ys)] ++ (map (\l -> y:l) (inserts x ys))  

permutations :: [a] -> [[a]]
permutations (x:[]) = [[x]]
permutations (x:xs) = foldr (++) [] (map (\l -> inserts x l) (permutations xs))


-- vii) write a function nub, which takes as an argument a list and returns the same list without the repetitions (the elements shall appear in the same order yet)
--
-- > nub [8, 2, 5, 5, 6, 5, 8]
-- [8,2,5,6]
remove_if_exists :: Integer -> [Integer] -> [Integer]
remove_if_exists x [] = []
remove_if_exists x (y:ys) = if x == y then (remove_if_exists x ys) else y:(remove_if_exists x ys)

nub :: [Integer] -> [Integer]
nub (x:[]) = [x]
--nub (x:xs) = x:(remove_if_exists x (nub xs))
nub (x:xs) = x : (remove_if_exists x $ nub xs)


-- viii) write a function fibo, which takes as an argument an integer n, and returns the nth fibonacci number?
--
-- > fibo 0
-- 1
--
-- > fibo 3
-- 5
--
-- what are fibo 10? fibo 100?

fibos 1 = (1,1)
fibos n = (n1+n2, n1) where (n1, n2) = fibos(n-1)

fibo 0 = 1
fibo 1 = 1
fibo n = fst (fibos n)


-- iv ) write a function packing, which packs consecutive equal elements in a list into sublists
--
-- > packing [2, 3, 3, 6, 5, 4, 4, 3]
-- [[2], [3, 3], [6], [5], [4, 4], [3]]

packing (x:[]) = [[x]]
packing (x:xs) = if x == head ys then (x:ys) : yss else [x]:ys:yss
                 where ys:yss = packing xs

-- v) write a function multiply which takes as arguments an integer n, a list l, and returns the list l in which each element appears n times
--
-- > multiply 2 [0, 7, 9]
-- [0, 0, 7, 7, 9, 9]
multiply :: Integer -> [x] -> [x]
multiply 0 _ = []
multiply _ [] = []
multiply n [x] = x:(multiply (n-1) [x])
multiply n (x:xs) = multiply n [x] ++ multiply n xs




















