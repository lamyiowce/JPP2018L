data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Ord)

instance Eq a => Eq (Tree a) where
    Empty == Empty = True
    Empty == Node _ _ _ = False
    Node _ _ _ == Empty = False
    Node x xbl xbr == Node y ybl ybr = x == y && xbl == ybl && xbr == ybr

instance Show a => Show (Tree a) where
    show Empty = ""
    show (Node x bl br) = (show x) ++ ":{" ++ (show bl) ++ "," ++ (show br) ++ "}"

instance Functor Tree where
--    fmap :: (a -> b) -> f a -> f b
    fmap fun Empty = Empty
    fmap fun (Node x bl br) = Node (fun x) (fmap fun bl) (fmap fun br)

toList :: Tree a -> [a]
toList Empty = []
toList (Node x bl br) = x:toList bl ++ toList br

my_insert :: (Ord a) => a -> Tree a -> Tree a
my_insert y Empty = Node y Empty Empty
my_insert y (Node x bl br)
    | y == x = Node x bl br
    | y < x = Node x (my_insert y bl) br
    | otherwise = Node x bl (my_insert y br)

contains :: (Ord a) => a -> Tree a -> Bool
contains _ Empty = False
contains y (Node x bl br)
    | y == x = True
    | y < x = contains y bl
    | otherwise = contains y br

fromList :: (Ord a) => [a] -> Tree a
fromList [] = Empty
fromList (h:t) = my_insert h (fromList t)


--import Prelude hiding(Either(..))

data MyEither a b = MyLeft a | MyRight b deriving (Show, Eq)

instance Functor (MyEither e) where
    fmap _ (MyLeft a) = MyLeft a
    fmap fun (MyRight a) = MyRight (fun a)

reverseRight :: MyEither e [a] -> MyEither e [a]
reverseRight (MyLeft a) = MyLeft a
reverseRight (MyRight a) = MyRight (reverse a)

reverse2 :: MyEither e [a] -> MyEither e [a]
reverse2 = fmap (reverse)

class Functor f => Pointed f where
  mypure :: a -> f a

instance Pointed Tree where 
  mypure x = Node x Empty Empty

instance Pointed [] where
  mypure x = [x]

instance Pointed (Either a) where
  mypure x = Right x


data Exp 
  = EInt Int             -- stała całkowita       
  | EAdd Exp Exp         -- e1 + e2
  | ESub Exp Exp         -- e1 - e2
  | EMul Exp Exp         -- e1 * e2
  | EVar String          -- zmienna
  | ELet String Exp Exp  -- let var = e1 in e2 

instance Eq Exp where
	EInt x == EInt y = x == y
	EVar x == EVar y = x == y
	EAdd x y == EAdd q w = x == q && y == w
	ESub x y == ESub q w = x == q && y == w
	EMul x y == EMul q w = x == q && y == w
	ELet s x y == ELet d q w = s == d && q == x && w == y
	_ == _ = False

instance Show Exp where
	show (EInt x) = show x
	show (EVar s) = show s
	show (EAdd x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
	show (ESub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
	show (EMul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
	show (ELet s x y) = "let " ++ s ++ " = " ++ show x ++ " in " ++ show y

instance Num Exp where
	(+) a b = EAdd a b
	(-) a b = ESub a b
	(*) a b = EMul a b
	abs a = undefined
	signum a = undefined
	fromInteger a = EInt $ fromIntegral a


simpl :: Exp -> Exp
simpl e = case e of 
	EAdd a b -> case as bs of 
		EAdd (EInt 0) bs -> simpl bs 
		EAdd as (EInt 0) -> simpl as 
		EAdd as bs -> EAdd (simpl as) (simpl bs)
		where as = simpl a 
	EMul (EInt 0) b -> EInt 0
	EMul a (EInt 0) -> EInt 0
	EMul (EInt 1) b -> simpl b
	EMul a (EInt 1) -> simpl a
	ESub a (EInt 0) -> simpl a
	EInt a -> EInt a
	EVar a -> EVar a

	 a -> a



