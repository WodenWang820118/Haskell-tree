------------------------ tree strucutre ------------------------
-- define the data strucutre
-- deriving Show to show Node in GHCI
data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show

-- | tree_insert takes any number and
--   and return a Node
tree_insert :: Ord a => a -> Tree a -> Tree a
tree_insert x Nil = Node x Nil Nil -- build a Node
tree_insert x (Node root left right)
  | x == root = Node root left right -- avoid duplicate
  | x < root = Node root (tree_insert x left) right -- insert a node on a left
  | x > root = Node root left (tree_insert x right) -- insert a node on the right

-- | tree_list_insert takes a list of numbers
--   and return a tree using tree_insert function
tree_list_insert :: (Ord a) => [a] -> Tree a
-- use the lambda function here; acc accumulates the tree
-- each time insert a node from the leftmost list 
-- for example [7,12,6,4,8] would start from 7
-- foldl function Nil List
tree_list_insert xs = foldl (\acc a -> tree_insert a acc) Nil xs

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
tree_average :: Tree Double -> Double
tree_average Nil = 0.0 -- empty tree
tree_average tree =
  let (sum1,count1) = tree_average' tree
  in sum1/count1

-- | helper function for calculating the average
--   which take a tree and return its node summation and counts
tree_average' :: Tree Double -> (Double,Double)
tree_average' Nil = (0.0,0.0)
tree_average' (Node root left right) =
  let (sumL,countL) = tree_average' left
      (sumR,countR) = tree_average' right
  in ((root+sumL+sumR),(1+countL+countR))

------------------------ Tree average ends ------------------------

------------------------ Tree delete node ------------------------

-- | delete_node takes a key and a tree
--   the function finds the key matches the node
--   and pass the key and tree to the helper function delete_node'
delete_node :: Ord a => a -> Tree a -> Tree a
delete_node _ Nil = Nil
delete_node x tree@(Node root left right)
  | x < root = Node root (delete_node x left) right
  | x > root = Node root left (delete_node x right)
  | x == root = delete_node' tree


-- for example
--     7
--    / \
--   6  Nil
--  / \
-- Nil Nil
--
-- delete the node 7 would be
--     6
--    / \
--   Nil Nil
-- which bring the left subtree as new root
--
-- | delete_node' takes a tree
--   the key already matched the root value
--   in thie function, to delete a node
--   bring the left subtree or right subtree as root if the other subtree is Nil
--   otherwise, use right subtree, retrieve its left_most_elem as a newRoot
delete_node' :: Ord a => Tree a -> Tree a
delete_node' (Node root Nil right) = right -- bring the right subtree as new root
delete_node' (Node root left Nil) = left -- bring the left subtree as new root
delete_node' (Node root left right) = (Node newRoot left right) where
  newRoot = left_most_elem right -- or the right most elemnet of the left subtree

left_most_elem :: Ord a => Tree a -> a
left_most_elem (Node root Nil _) = root -- there's no more left subtree, get its root value
left_most_elem (Node _ left _) = left_most_elem left -- otherwise, keep search its left subtree

------------------------ Tree delete node ends ------------------------