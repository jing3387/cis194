module Golf where

import Control.Category

takeEveryNth :: Int -> [a] -> [a]
-- Why does left-to-right composition have a verbose operator? Left here as a
-- note of how to do it.
takeEveryNth n = drop (n - 1) >>> takeEveryNth' where
  takeEveryNth' :: [a] -> [a]
  takeEveryNth' [] = []
  takeEveryNth' (x:ys) = x : takeEveryNth' (drop (n - 1) ys)

skips :: [a] -> [[a]]
skips xs = skips' 1 xs where
  skips' :: Int -> [a] -> [[a]]
  skips' m ys
    | m == length xs = [ys] -- Is length xs optimised out?
    | otherwise = takeEveryNth m ys : skips' (m + 1) ys

-- Alternative version.
skips'' :: [a] -> [[a]]
skips'' l = map (f l) [1 .. length l] where
  f :: [a] -> Int -> [a]
  f l' n = case drop (n - 1) l' of
    (x:xs) -> x : f xs n
    [] -> []

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:rest)
  | a < b && b > c = b : localMaxima (c:rest)
  | otherwise = localMaxima $ b:c:rest
localMaxima _ = []
