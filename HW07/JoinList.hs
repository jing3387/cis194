{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Data.Monoid
import Data.Char
import Sized
import Scrabble
import Buffer
import Editor

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

scoreString :: String -> Score
scoreString s = foldr (+) 0 (map (score . toUpper) s)

scoreLine :: String -> JoinList Score String
scoreLine l = Single (scoreString l) l

scoreSizeLine :: String -> JoinList (Score, Size) String
scoreSizeLine l = Single (scoreString l, 1) l

toListJ :: JoinList m a -> [a]
toListJ Empty = []
toListJ (Single _ a) = [a]
toListJ (Append _ a b) = toListJ a ++ toListJ b

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . toListJ
  fromString = foldr (\x acc -> scoreSizeLine x +++ acc) Empty . lines
  line = indexJ 
  replaceLine n s b =
    let before = takeJ n b
        after = dropJ (n + 1) b
     in before +++ fromString s +++ after
  numLines = getSize . snd . tag
  value = getScore . fst . tag

main = runEditor editor scratch
  where 
    scratch :: JoinList (Score, Size) String
    scratch = 
      fromString . unlines $ 
        [ "This buffer is for notes you don't want to save, and for"
        , "evaluation of steam valve coefficients."
        , "To load a different file, type the character L followed"
        , "by the name of the file."
        ]
