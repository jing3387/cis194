{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

newtype Score = Score { getScore :: Int }
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score c
  | elem c "AEILNORST" = 1
  | elem c "DG" = 2
  | elem c "BCMP" = 3
  | elem c "FHVWY" = 4
  | c == 'K' = 5
  | elem c "JX" = 8
  | elem c "QZ" = 10
  | otherwise = 0
