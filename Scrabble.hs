{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid
import Data.Char (toLower)

data Score = Score { getScore :: Int } deriving (Show)

instance Monoid Score where
    mempty = Score 0
    mappend (Score x) (Score y) = Score (x+y)

score :: Char -> Score
score x = Score . toScore . toLower $ x where
    toScore y
        | elem y "aeilnorstu" = 1
        | elem y "dg" = 2
        | elem y "bcmp" = 3
        | elem y "fhvwy" = 4
        | elem y "k" = 5
        | elem y "jx" = 8
        | elem y "qz" = 10
        | otherwise = 0

scoreString :: String -> Score
scoreString = mconcat . map score
