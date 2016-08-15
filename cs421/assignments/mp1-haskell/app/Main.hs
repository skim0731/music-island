module Main where

-- mytake
-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake _ []     = []
mytake a (x:xs) | a > 0     = x : (mytake (a-1) xs) 
				| otherwise = [] 

-- mydrop
-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop a (x:xs) | a > 0     = mydrop (a-1) xs
				| otherwise = (x:xs)

-- rev
-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a] 
rev [] = []
rev l = _rev l [] 
	where 
		_rev :: [a] -> [a] -> [a]
		_rev []     l = l
		_rev (x:xs) l = _rev xs (x:l) 

-- app
-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app []     [] = []
app x      [] = x
app []     y  = y
app (x:xs) y  = x : (app xs y) 

-- inclist
-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist []      = [] 
inclist (x:xs)  = x+1 : inclist xs  

-- sumlist
-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist l = aux l 0
	where 
		aux []     a = a 
		aux (x:xs) a = aux xs (a+x)

-- myzip
-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip (x:xs) (y:ys) = (x,y) : (myzip xs ys)
myzip _      _      = [] 

-- addpairs
-- don't forget to put the type declaration or you will lose points!
addpairs :: Num a => [a] -> [a] -> [a]
addpairs a b = aux (myzip a b)
	where
		aux []         = []
		aux ((a,b):xs) = (a+b) : aux xs

-- ones
-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1 : ones 

-- nats
-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = 0 : map (+1) nats 

-- fib
-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0 : 1 : addpairs fib (tail fib)
-- add
-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add a []             = [a] 
add a (x:xs) 
	| a > x     = x : add a xs
	| a == x    = x : xs
	| otherwise = a : x : xs 

-- union
-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union []     []     = []
union x      []     = x
union []     y      = y 
union (x:xs) (y:ys) 
	| x < y     = x : union xs     (y:ys)
	| x == y    = x : union xs     ys 
	| otherwise = y : union (x:xs) ys  

-- intersect
-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect (x:xs) (y:ys)
	| x < y     = intersect xs (y:ys)
	| x == y    = x : intersect xs ys
	| otherwise = intersect (x:xs) ys 
intersect _      _      = [] 

-- powerset
-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset []     = [[]] 
powerset (x:xs) = 
	let 
		subp = powerset xs
	in 
		([] : (_add x subp)) ++ tail subp
	where
		_add a []     = [] 
		_add a (x:xs) = (a:x) : (_add a xs)

-- inclist'
-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' l = map (+1) l 
 
-- sumlist'
-- don't forget to put the type declaration or you will lose points!
sumlist' :: Num a => [a] -> a
sumlist' l = foldr (+) 0 l

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

-- list2cons
-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a
list2cons []     = Nil 
list2cons (x:xs) = Cons x (list2cons xs)

-- cons2list
-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil           = []
cons2list (Cons a b) = a : cons2list b

-- eval
-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer
eval (IntExp  i)  = i
eval (PlusExp []) = 0 
eval (MultExp []) = 1
eval (PlusExp (x:xs)) = eval x + eval(PlusExp xs)
eval (MultExp (x:xs)) = eval x * eval(MultExp xs) 

-- list2cons'
-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' l = foldr (\a -> (Cons a)) Nil l

-- BinTree
data BinTree a = Node a (BinTree a) (BinTree a)
			| Leaf
	deriving (Show)


-- sumTree
-- don't forget to put the type declaration or you will lose points!
sumTree :: Num a => BinTree a -> a 
sumTree (Leaf)         = 0
sumTree (Node a t1 t2) = a + sumTree t1 + sumTree t2

-- SimpVal
data SimpVal = IntVal Integer
			| BoolVal Bool
			| StrVal String
			| ExnVal String
		deriving (Show)

-- liftIntOp
-- don't forget to put the type declaration or you will lose points!
liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp f (IntVal a) (IntVal b) = IntVal (f a b)
liftIntOp f _          _          = ExnVal ("not an IntVal!")