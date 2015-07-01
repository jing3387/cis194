module JoinList where

import Data.Monoid
import Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

example :: JoinList Size Char
example =
  Append (Size 4)
    (Append (Size 3)
      (Single (Size 1) 'y')
      (Append (Size 2)
        (Single (Size 1) 'e')
        (Single (Size 1) 'a')))
    (Single (Size 1) 'h')

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
x +++ y = Append (tag x <> tag y) x y

sizeJ :: (Sized b, Monoid b) => JoinList b a -> Int
sizeJ = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i x
  | i < 0 = Nothing
  | i >= sizeJ x = Nothing
indexJ 0 (Single _ x) = Just x
indexJ i (Append _ x y)
  | i < sx = indexJ i x
  | otherwise = indexJ (i - sx) y
  where sx = sizeJ x

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n x
  | n <= 0 = x
  | n >= sizeJ x = Empty
dropJ n (Append _ l r)
  | n < sl = dropJ n l +++ r
  | otherwise = dropJ (n - sl) r
  where sl = sizeJ l

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n _ | n <= 0 = Empty
takeJ n x | n >= sizeJ x = x
takeJ n (Append _ l r)
  | n < sl = takeJ n l
  | otherwise = l +++ takeJ (n - sl) r
  where sl = sizeJ l
