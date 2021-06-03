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
  | x > root = Node root left (treeInsert x right) -- insert a node on the right
  | x < root = Node root (treeInsert x left) right -- insert a node on a left

-- | treeListInsert takes a list of numbers
--   and return a tree using treeInsert function
treeListInsert :: (Ord a) => [a] -> Tree a
treeListInsert [] = Nil -- empty list return Nil
treeListInsert (x:xs) = treeInsert x (treeListInsert xs) -- iterate through the list and insert x to tree repeatedly

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