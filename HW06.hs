{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module HW06 where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = map fst . iterate (\(i,j) -> (j,i+j)) $ (0,1)

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons a as) = a : streamToList as

streamRepeat :: a -> Stream a
streamRepeat a = (Cons a (streamRepeat a))

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a as) = Cons (f a) (streamMap f as)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

nat :: Stream Integer
nat = streamFromSeed (\a -> a + 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) bs = Cons a (interleaveStreams bs as)

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)

x :: Stream Integer
x = Cons 0 (Cons 1 $ streamRepeat 0)

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap negate
  (Cons a as) + (Cons b bs) = Cons (a + b) (as + bs)
  (Cons a as) * bbs@(Cons b bs) = Cons (a * b) ((streamMap (* a) bs) + (as * bbs))

instance Fractional (Stream Integer) where
  (Cons a as) / (Cons b bs) = qs
    where qs = Cons (a `div` b) (streamMap (`div` b) (as - qs * bs))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)
