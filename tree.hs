
-- 1. lambda functions (from sample exam)
-- e.g. [1,2,3,2,1] -> [1,4,9,4,1]
-- map (\x -> x*x ) List
-- map (\x -> (^2) x) List

-- https://wiki.haskell.org/List_comprehension
-- 2. list comprehension (from sample exam)
-- e.g. coefficientList = [1,3,5]
-- x is the variable
-- ask for the summation of the list
-- sum $ [coefficient * (^exponential) x | (coefficient, exponential) <- zip coefficientList [0..]]
-- this is just a expression, but not executable

-- list comprehension
-- basic usage
-- [(i,j) | i <- [1..4], j <- [1,2]]

-- list comprehension with other functions
-- e.g. take
-- take 10 [(i,j) | i <- [1..], j <- [1..i-1]]

-- list comprehension with Monad
-- do { i <- [1,2] ; j <- [1..4]; return (i, j) }

------------------------ other functions ------------------------
-- dropWhile
-- http://zvon.org/other/haskell/Outputprelude/dropWhile_f.html
-- dropWhile function drops the element with conditions
-- and return the rest of the list (sample exam)
-- dropWhile (==0) [0,0,0,2,0,7,0,0] -> [2,0,7,0,0]
-- reverse $ dropWhile (==0) $ reverse $ dropWhile (==0)

-- dropWhile could be used with lambda functions
-- dropWhile (\x -> 6*x < 100) [1..20] -> [17,18,19,20]


-- https://wiki.haskell.org/Fold

-- foldr
-- https://stackoverflow.com/questions/1757740/how-does-foldr-work
-- foldr (-) 54 [10, 11] -> 53
-- foldr (\x y -> (x+y)/2) 54 [12, 4, 10, 6] -> 12.0

--foldl work the same way but from the left, cannot deal with infinite loop like foldr

-- if the list is empty, the result is the initial value z; else
-- apply f to the first element and the result of folding the rest
-- foldr :: (a -> a -> b) -> b -> [a] -> b
-- foldr f z []     = z 
-- foldr f z (x:xs) = f x (foldr f z xs) 

-- if the list is empty, the result is the initial value; else
-- we recurse immediately, making the new initial value the result
-- of combining the old initial value with the first element.
-- foldl f z []     = z                  
-- foldl f z (x:xs) = foldl f (f z x) xs

------------------------ tree strucutre ------------------------
-- define the data strucutre
-- deriving Show to show Node in GHCI
data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show

-- | treeInsert takes any number and
--   and return a Node
treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Nil = Node x Nil Nil -- build a Node
treeInsert x (Node root left right)
  | x == root = Node root left right -- avoid duplicate
  | x < root = Node root (treeInsert x left) right -- insert a node on a left
  | x > root = Node root left (treeInsert x right) -- insert a node on the right

-- | treeListInsert takes a list of numbers
--   and return a tree using treeInsert function
treeListInsert :: (Ord a) => [a] -> Tree a
-- use the lambda function here; acc accumulates the tree
-- each time insert a node from the leftmost list 
-- for example [7,12,6,4,8] would start from 7
-- foldl function Nil List
treeListInsert xs = foldl (\acc a -> treeInsert a acc) Nil xs

------------------------ Traversal ------------------------

inorder :: Tree a -> [a]
inorder Nil = []
inorder (Node root left right) = inorder left ++ [root] ++ inorder right

preorder :: Tree a -> [a]
preorder Nil = []
preorder (Node root left right) = [root] ++ preorder left ++ preorder right

postorder :: Tree a-> [a]
postorder Nil = []
postorder (Node root left right) = postorder left ++ postorder right ++ [root]

------------------------ Traversal ends ------------------------
------------------------ Tree height ------------------------
-- | get a Tree and return its height
height :: Tree a -> Int
height Nil = 0
height (Node root left right) = 1 + max (height left) (height right)
------------------------ Tree height ends ------------------------

-- | get a Tree and calculate its height
--   to see if the tree is balanced
is_balance :: Tree a -> Bool
is_balance Nil = True
is_balance (Node root left right)
  | abs (height left - height right) <= 1 = True
  | otherwise = False 