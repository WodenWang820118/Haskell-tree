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

------------------------ Tree balance ------------------------
-- | get a Tree and calculate its height
--   to see if the tree is balanced
is_balance :: Tree a -> Bool
is_balance Nil = True
is_balance (Node root left right)
  | abs (height left - height right) <= 1 = True
  | otherwise = False 
------------------------ Tree balance ends ------------------------

------------------------ Tree average ------------------------
-- | get the double value from a tree
--   return the average
treeAverage :: Tree Double -> Double
treeAverage Nil = 0.0 -- empty tree
treeAverage tree =
  let (sum1,count1) = treeAverage' tree
  in sum1/count1

-- | helper function for calculating the average
--   which take a tree and return its node summation and counts
treeAverage' :: Tree Double -> (Double,Double)
treeAverage' Nil = (0.0,0.0)
treeAverage' (Node root left right) =
  let (sumL,countL) = treeAverage' left
      (sumR,countR) = treeAverage' right
  in ((root+sumL+sumR),(1+countL+countR))

------------------------ Tree average ends ------------------------