module HW04 where

import Data.List hiding (insert)

fun1 :: [Integer] -> Integer
fun1 = foldr (\x -> (*) (x - 2)) 1 . filter even

fun1' :: [Integer] -> Integer
fun1' [] = 1
fun1' (x:xs)
  | even x = (x - 2) * fun1' xs
  | otherwise = fun1' xs

fun2 :: Integer -> Integer
fun2 = sum . takeWhile (/= 1) . drop 1 . iterate (\n ->
             if even n then n `div` 2 else 3 * n + 1)

fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' n
  | even n = n + fun2' (n `div` 2)
  | otherwise = fun2' (3 * n + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node _ Leaf y Leaf) = Node 1 (insert x Leaf) y Leaf
insert x (Node _ l y Leaf) = Node 1 l y (insert x Leaf)
insert x (Node _ Leaf y r) = Node 1 (insert x Leaf) y r
insert x (Node h l@(Node hl _ _ _) y r@(Node hr _ _ _))
  -- We can insert into the left without changing the height of the tree.
  | hl < hr = Node (hr+1) (insert x l) y r
  -- Try and insert into the right. If it's height is the same as the root
  -- update the root's height appropriately.
  | otherwise =
      case insert x r of
        r'@(Node hr' _ _ _)
          | h == hr' -> Node (h+1) l y r'
          | otherwise -> Node h l y r'
        _ -> Node h l y r

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

fromBool :: Bool -> Integer
fromBool False = 0
fromBool True = 1

xor :: [Bool] -> Bool
xor = odd . sum . map fromBool

xor' :: [Bool] -> Bool
xor' = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

exclude :: Integer -> [Integer]
exclude n = [k | i <- [1..n], j <- [1..n], i <= j, let k = i+j+2*i*j, k <= n]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) $ [1..n] \\ exclude n
